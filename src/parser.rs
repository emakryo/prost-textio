use std::{borrow::Cow, cell::RefCell, collections::HashMap, io::Read, rc::Rc};

use prost::{bytes::Bytes, Message};
use prost_reflect::{
    DynamicMessage, ExtensionDescriptor, FieldDescriptor, FileDescriptor, Kind, MessageDescriptor,
    OneofDescriptor, ReflectMessage, Value,
};
use prost_types::FieldDescriptorProto;

use crate::{
    tokenizer::{parse_integer, parse_string_append, ErrorCollector, TokenType, Tokenizer},
    ProtoError, Result,
};

#[derive(Clone)]
pub struct Parser {
    opts: ParserOpts,
    error_collector: Rc<RefCell<dyn ErrorCollector>>,
    finder: Finder,
    parse_info_tree: Option<Rc<RefCell<ParseInfoTree>>>,
}

impl Default for Parser {
    fn default() -> Self {
        Self {
            opts: ParserOpts::default(),
            error_collector: Rc::new(RefCell::new(())),
            finder: Finder::new(),
            parse_info_tree: None,
        }
    }
}

impl Parser {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn parse<R: Read>(&self, reader: R, output: &mut DynamicMessage) -> bool {
        output.clear();
        let mut parser = ParserImpl::new(
            reader,
            Rc::clone(&self.error_collector),
            self.finder.clone(),
            self.parse_info_tree.clone(),
            self.opts.clone(),
        );

        self.merge_using_impl(output, &mut parser)
    }

    pub fn parse_from_str(&self, input: &str, message: &mut DynamicMessage) -> bool {
        self.parse(input.as_bytes(), message)
    }

    fn merge_using_impl<R: Read>(
        &self,
        output: &mut DynamicMessage,
        parser_impl: &mut ParserImpl<R>,
    ) -> bool {
        if parser_impl.parse(output).is_err() {
            return false;
        }
        if !self.opts.allow_partial && !output.is_initialized() {
            let missing_fields = output.find_initialization_errors();
            parser_impl.report_error_pos(
                0,
                0,
                &format!(
                    "Message missing required fields: {}",
                    missing_fields.join(",")
                ),
            );
            return false;
        }
        true
    }

    pub fn merge<R: Read>(&self, reader: R, output: &mut DynamicMessage) -> bool {
        let mut opts = self.opts.clone();
        opts.singular_overwrite_policy = SingularOverwritePolicy::Allow;

        let mut parser = ParserImpl::new(
            reader,
            Rc::clone(&self.error_collector),
            self.finder.clone(),
            self.parse_info_tree.clone(),
            opts,
        );

        self.merge_using_impl(output, &mut parser)
    }

    pub fn merge_from_str(&self, input: &str, message: &mut DynamicMessage) -> bool {
        self.merge(input.as_bytes(), message)
    }

    pub fn write_localtions_to(&mut self, info_tree: Rc<RefCell<ParseInfoTree>>) {
        self.parse_info_tree = Some(info_tree);
    }

    pub fn record_error_to(&mut self, error_collector: Rc<RefCell<dyn ErrorCollector>>) {
        self.error_collector = error_collector;
    }

    fn parse_field_value_from_string(
        &self,
        input: &str,
        field: &FieldDescriptor,
        message: &mut DynamicMessage,
    ) -> bool {
        let mut parser = ParserImpl::new(
            input.as_bytes(),
            Rc::clone(&self.error_collector),
            self.finder.clone(),
            self.parse_info_tree.clone(),
            self.opts.clone(),
        );

        parser.parse_field(field, message).is_ok()
    }

    pub fn allow_case_insensitive_field(&mut self, allow: bool) {
        self.opts.allow_case_insensitive_field = allow;
    }

    pub fn allow_partial_message(&mut self, allow: bool) {
        self.opts.allow_partial = allow;
    }

    pub fn allow_unknown_field(&mut self, allow: bool) {
        self.opts.allow_unknown_field = allow;
    }

    pub fn allow_unknown_extension(&mut self, allow: bool) {
        self.opts.allow_unknown_extension = allow;
    }

    pub fn allow_field_number(&mut self, allow: bool) {
        self.opts.allow_field_number = allow;
    }

    pub fn set_recursion_limit(&mut self, limit: usize) {
        self.opts.recursion_limit = limit;
    }

    pub fn set_fineder(&mut self, finder: Finder) {
        self.finder = finder
    }
}

pub fn parse_field_value_from_string(
    input: &str,
    field: &FieldDescriptor,
    message: &mut DynamicMessage,
) -> bool {
    Parser::new().parse_field_value_from_string(input, field, message)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SingularOverwritePolicy {
    Allow,
    Forbid,
}

#[derive(Clone, Default)]
pub struct Finder {
    descriptors: Vec<FileDescriptor>,
}

impl Finder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_descriptors(file_descriptor: &[&FileDescriptor]) -> Self {
        Self {
            descriptors: file_descriptor.iter().map(|&x| x.to_owned()).collect(),
        }
    }

    fn find_extension(&self, message: &DynamicMessage, name: &str) -> Option<ExtensionDescriptor> {
        let descriptor = message.descriptor();
        let ret = descriptor.extensions().find(|e| e.full_name() == name);

        ret
    }

    fn find_any_type(
        &self,
        message: &DynamicMessage,
        prefix: &str,
        name: &str,
    ) -> Option<MessageDescriptor> {
        if prefix != "type.googleapis.com/" && prefix != "type.googleprod.com/" {
            None
        } else {
            message
                .descriptor()
                .parent_file()
                .get_message_by_name(name)
                .or_else(|| {
                    for desc in &self.descriptors {
                        let message = desc.get_message_by_name(name);
                        if message.is_some() {
                            return message;
                        }
                    }
                    None
                })
        }
    }

    fn find_extension_by_number(
        &self,
        descriptor: &MessageDescriptor,
        number: u32,
    ) -> Option<ExtensionDescriptor> {
        descriptor.get_extension(number)
    }
}

#[derive(Clone, Default)]
pub struct ParseInfoTree {
    locations: HashMap<String, Vec<ParseLocationRange>>,
    nested: HashMap<String, Vec<Rc<RefCell<ParseInfoTree>>>>,
}

impl ParseInfoTree {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self::default()))
    }

    fn record_location(&mut self, field: &FieldExtensionDescriptor, location: ParseLocationRange) {
        self.locations
            .entry(field.full_name().to_string())
            .or_default()
            .push(location);
    }

    fn create_nested(&mut self, field: &FieldExtensionDescriptor) -> Rc<RefCell<Self>> {
        let entry = self
            .nested
            .entry(field.full_name().to_string())
            .or_default();
        entry.push(Self::new());
        Rc::clone(entry.last().unwrap())
    }

    fn check_field_index(&self, field: &FieldDescriptor, index: usize) {
        assert!(field.is_list() || index == 0);
    }

    pub fn get_location_range(
        &self,
        field: &FieldDescriptor,
        index: usize,
    ) -> Option<&ParseLocationRange> {
        self.check_field_index(field, index);

        if let Some(locations) = self.locations.get(field.full_name()) {
            locations.get(index)
        } else {
            None
        }
    }

    pub fn get_location(&self, field: &FieldDescriptor, index: usize) -> Option<&ParseLocation> {
        self.get_location_range(field, index).map(|rng| &rng.start)
    }

    pub fn get_tree_for_nested(
        &self,
        field: &FieldDescriptor,
        index: usize,
    ) -> Option<Rc<RefCell<ParseInfoTree>>> {
        self.check_field_index(field, index);

        if let Some(nested) = self.nested.get(field.full_name()) {
            nested.get(index).map(Rc::clone)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseLocation {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseLocationRange {
    pub start: ParseLocation,
    pub end: ParseLocation,
}

impl From<(usize, usize, usize, usize)> for ParseLocationRange {
    fn from(
        (start_line, start_column, end_line, end_column): (usize, usize, usize, usize),
    ) -> Self {
        Self {
            start: ParseLocation {
                line: start_line,
                column: start_column,
            },
            end: ParseLocation {
                line: end_line,
                column: end_column,
            },
        }
    }
}

#[derive(Clone)]
struct ParserOpts {
    singular_overwrite_policy: SingularOverwritePolicy,
    allow_case_insensitive_field: bool,
    allow_unknown_field: bool,
    allow_unknown_extension: bool,
    allow_unknown_enum: bool,
    allow_relaxed_whitespace: bool,
    allow_field_number: bool,
    allow_partial: bool,
    recursion_limit: usize,
}

impl Default for ParserOpts {
    fn default() -> Self {
        Self {
            singular_overwrite_policy: SingularOverwritePolicy::Forbid,
            allow_partial: false,
            allow_case_insensitive_field: false,
            allow_unknown_field: false,
            allow_unknown_extension: false,
            allow_unknown_enum: false,
            allow_field_number: false,
            allow_relaxed_whitespace: false,
            recursion_limit: u32::MAX as usize,
        }
    }
}

enum FieldExtensionDescriptor {
    Field(FieldDescriptor),
    Extension(ExtensionDescriptor),
}

impl From<FieldDescriptor> for FieldExtensionDescriptor {
    fn from(x: FieldDescriptor) -> Self {
        Self::Field(x)
    }
}

impl From<ExtensionDescriptor> for FieldExtensionDescriptor {
    fn from(x: ExtensionDescriptor) -> Self {
        Self::Extension(x)
    }
}

impl FieldExtensionDescriptor {
    fn is_group(&self) -> bool {
        match self {
            Self::Field(x) => x.is_group(),
            Self::Extension(x) => x.is_group(),
        }
    }

    fn is_list(&self) -> bool {
        match self {
            Self::Field(x) => x.is_list(),
            Self::Extension(x) => x.is_list(),
        }
    }

    fn kind(&self) -> Kind {
        match self {
            Self::Field(x) => x.kind(),
            Self::Extension(x) => x.kind(),
        }
    }

    fn containing_oneof(&self) -> Option<OneofDescriptor> {
        match self {
            Self::Field(x) => x.containing_oneof(),
            _ => None,
        }
    }

    fn field_descriptor_proto(&self) -> &FieldDescriptorProto {
        match self {
            Self::Field(x) => x.field_descriptor_proto(),
            Self::Extension(x) => x.field_descriptor_proto(),
        }
    }

    fn belongs_to(&self, message: &DynamicMessage) -> bool {
        match self {
            Self::Field(x) => message.has_field(x),
            Self::Extension(x) => message.has_extension(x),
        }
    }

    fn name(&self) -> &str {
        match self {
            Self::Field(x) => x.name(),
            Self::Extension(x) => x.name(),
        }
    }

    fn full_name(&self) -> &str {
        match self {
            Self::Field(x) => x.full_name(),
            Self::Extension(x) => x.full_name(),
        }
    }

    #[allow(unused)]
    fn value_of<'a>(&self, message: &'a DynamicMessage) -> Cow<'a, Value> {
        match self {
            Self::Field(x) => message.get_field(x),
            Self::Extension(x) => message.get_extension(x),
        }
    }

    fn mut_value_of<'a>(&self, message: &'a mut DynamicMessage) -> &'a mut Value {
        match self {
            Self::Field(x) => message.get_field_mut(x),
            Self::Extension(x) => message.get_extension_mut(x),
        }
    }

    fn set_into(&self, value: Value, message: &mut DynamicMessage) {
        match self {
            Self::Field(x) => message.set_field(x, value),
            Self::Extension(x) => message.set_extension(x, value),
        }
    }
}

struct ParserImpl<R> {
    error_collector: Rc<RefCell<dyn ErrorCollector>>,
    finder: Finder,
    parse_info_tree: Option<Rc<RefCell<ParseInfoTree>>>,
    opts: ParserOpts,
    tokenizer: Tokenizer<R>,
    had_errors: bool,
    init_recursion_limit: usize,
}

impl<'a, R: Read> ParserImpl<R> {
    fn new(
        reader: R,
        error_collector: Rc<RefCell<dyn ErrorCollector>>,
        finder: Finder,
        parse_info_tree: Option<Rc<RefCell<ParseInfoTree>>>,
        opts: ParserOpts,
    ) -> Self {
        let mut tokenizer = Tokenizer::new(reader, Rc::clone(&error_collector));
        tokenizer.set_allow_f_after_float(true);
        tokenizer.set_comment_style(super::tokenizer::CommentStyle::ShellCommentStyle);

        if opts.allow_relaxed_whitespace {
            tokenizer.set_require_space_after_number(false);
            tokenizer.set_allow_multiline_strings(true);
        }
        tokenizer.next();
        let init_recursion_limit = opts.recursion_limit;
        Self {
            error_collector,
            finder,
            parse_info_tree,
            opts,
            tokenizer,
            had_errors: false,
            init_recursion_limit,
        }
    }

    fn parse(&mut self, message: &mut DynamicMessage) -> Result<()> {
        loop {
            if self.looking_at_type(TokenType::Eof) {
                if self.had_errors {
                    todo!()
                } else {
                    return Ok(());
                }
            }

            self.consume_field(message)?;
        }
    }

    fn parse_field(&mut self, field: &FieldDescriptor, message: &mut DynamicMessage) -> Result<()> {
        if let Kind::Message(_) = field.kind() {
            self.consume_field_message(message, &field.to_owned().into())?
        } else {
            self.consume_field_value(message, &field.to_owned().into())?
        };

        if !self.looking_at_type(TokenType::Eof) {
            return Err(ProtoError::Todo("Remaining tokens".into()));
        }
        Ok(())
    }

    fn report_error_pos(&mut self, line: usize, column: usize, message: &str) {
        self.had_errors = true;
        self.error_collector
            .borrow_mut()
            .add_error(line, column, message);
    }

    fn report_warning_pos(&mut self, line: usize, col: usize, message: &str) {
        self.error_collector
            .borrow_mut()
            .add_warning(line, col, message);
    }

    fn report_error(&mut self, message: &str) {
        self.report_error_pos(
            self.tokenizer.current().line,
            self.tokenizer.current().column,
            message,
        );
    }

    fn report_warning(&mut self, message: &str) {
        self.report_warning_pos(
            self.tokenizer.current().line,
            self.tokenizer.current().column,
            message,
        );
    }

    fn consume_message(&mut self, message: &mut DynamicMessage, delimiter: &str) -> Result<()> {
        while !self.looking_at(">") && !self.looking_at("}") {
            self.consume_field(message)?;
        }

        self.consume(delimiter)?;
        Ok(())
    }

    fn consume_message_delimiter(&mut self) -> Result<String> {
        if self.try_consume("<").is_ok() {
            Ok(">".into())
        } else {
            self.consume("{")?;
            Ok("}".into())
        }
    }

    fn decrease_recursion_limit(&mut self) -> Result<()> {
        if self.opts.recursion_limit == 0 {
            self.report_error(&format!(
                "Message is too deep, the parser exceeded the configured recursion limit of {}.",
                self.init_recursion_limit
            ));
            return Err(ProtoError::Todo("Recursion limit exceeded".into()));
        }
        self.opts.recursion_limit -= 1;
        Ok(())
    }

    fn consume_field(&mut self, message: &mut DynamicMessage) -> Result<()> {
        let descriptor = message.descriptor();

        let mut field: Option<FieldExtensionDescriptor>;
        let field_name;
        let mut reserved_field = false;
        let start_line = self.tokenizer.current().line;
        let start_column = self.tokenizer.current().column;

        if let Some((any_type_url_field, any_value_field)) = get_any_field_descriptors(message) {
            if self.try_consume("[").is_ok() {
                let (full_type_name, prefix) = self.consume_any_type_url()?;
                let mut prefix_and_full_type_name: String = prefix.clone();
                prefix_and_full_type_name.push_str(&full_type_name);
                self.consume_before_whitespace("]")?;
                self.try_consume_whitespace(&prefix_and_full_type_name, "Any")
                    .ok();
                self.try_consume_before_whitespace(":").ok();
                self.try_consume_whitespace(&prefix_and_full_type_name, "Any")
                    .ok();

                let value_descriptor = self
                    .finder
                    .find_any_type(message, &prefix, &full_type_name)
                    .ok_or_else(|| ProtoError::Todo("Any type not found".into()))?;

                let serialized_value = self.consume_any_value(&value_descriptor)?;

                if self.opts.singular_overwrite_policy == SingularOverwritePolicy::Forbid
                    && ((!any_type_url_field.is_list() && message.has_field(&any_type_url_field))
                        || (!any_value_field.is_list() && message.has_field(&any_value_field)))
                {
                    self.report_error("Non repeated Any specified multiple times");
                    return Err(ProtoError::Todo("Duplicated Any".into()));
                }

                message.set_field(
                    &any_type_url_field,
                    Value::String(prefix_and_full_type_name),
                );
                message.set_field(
                    &any_value_field,
                    Value::Bytes(Bytes::from(serialized_value)),
                );
                return Ok(());
            }
        }
        if self.try_consume("[").is_ok() {
            field_name = self.consume_full_type_name()?;
            self.consume_before_whitespace("]")?;
            self.try_consume_whitespace(descriptor.full_name(), "Extension")
                .ok();

            field = self
                .finder
                .find_extension(message, &field_name)
                .map(From::from);

            if field.is_none() {
                if !self.opts.allow_unknown_field && !self.opts.allow_unknown_extension {
                    self.report_error(&format!(
                        "Extension \"{}\" is not defined or is not an extension of \"{}\".",
                        field_name,
                        descriptor.full_name(),
                    ));
                    return Err(ProtoError::Todo("Unknown extension".into()));
                } else {
                    self.report_warning(&format!(
                        "Ignore extension {} which is not defined or is not an extension of {}.",
                        field_name,
                        descriptor.full_name(),
                    ))
                }
            }
        } else {
            field_name = self.consume_identifier_before_whitespace()?;
            self.try_consume_whitespace(descriptor.full_name(), "Normal")
                .ok();

            if let (Ok(field_number), true) =
                (field_name.parse::<u32>(), self.opts.allow_field_number)
            {
                if descriptor.is_extension_number(field_number) {
                    field = self
                        .finder
                        .find_extension_by_number(&descriptor, field_number)
                        .map(From::from);
                } else if descriptor.is_reserverd_field(field_number) {
                    field = None;
                } else {
                    field = descriptor.get_field(field_number).map(From::from);
                }
            } else {
                field = descriptor.get_field_by_name(&field_name).map(From::from);

                if field.is_none() {
                    let lower_field_name = field_name.to_ascii_lowercase();
                    field = descriptor
                        .get_field_by_name(&lower_field_name)
                        .map(From::from);

                    if let Some(f) = &field {
                        if !f.is_group() {
                            field = None;
                        }
                    }
                }

                if let Some(f) = &field {
                    if f.is_group() && f.kind().as_message().unwrap().name() != field_name {
                        field = None;
                    }
                }

                if field.is_none() && self.opts.allow_case_insensitive_field {
                    let lower_field_name = field_name.to_ascii_lowercase();
                    field = descriptor
                        .get_field_by_lower_case_name(&lower_field_name)
                        .map(From::from);
                }

                if field.is_none() {
                    reserved_field = descriptor.is_reserved_name(&field_name);
                }
            }

            if field.is_none() && !reserved_field {
                let msg = format!(
                    "Message type \"{}\" has no field named \"{}\".",
                    descriptor.full_name(),
                    field_name,
                );
                if !self.opts.allow_unknown_field {
                    self.report_error(&msg);
                    return Err(ProtoError::Todo("Unknown field".into()));
                } else {
                    self.report_warning(&msg);
                }
            }
        }

        if field.is_none() {
            assert!(
                self.opts.allow_unknown_field
                    || self.opts.allow_unknown_extension
                    || reserved_field
            );

            if self.try_consume_before_whitespace(":").is_ok() {
                self.try_consume_whitespace(descriptor.full_name(), "Unknown/Reserved")
                    .ok();
                if !self.looking_at("{") && !self.looking_at("<") {
                    return self.skip_field_value();
                }
            }

            return self.skip_field_message();
        }

        let field = field.unwrap();

        if self.opts.singular_overwrite_policy == SingularOverwritePolicy::Forbid {
            if !field.is_list() && field.belongs_to(message) {
                self.report_error(&format!(
                    "Non-repeated field \"{}\" is specified multiple times.",
                    field_name,
                ));
                return Err(ProtoError::Todo("Duplicated field".into()));
            }
            if let Some(oneof) = field.containing_oneof() {
                if message.has_oneof(&oneof) {
                    if let Some(other_field) = message.get_oneof_field_descriptor(&oneof) {
                        self.report_error(&format!(
                            "Field '{}' is specified along with field '{}', another member of oneof '{}'.",
                            &field_name, other_field.name(), oneof.name()
                        ));
                        return Err(ProtoError::Todo("Duplicaed oneof".into()));
                    }
                }
            }
        }

        (|| -> Result<()> {
            if let Kind::Message(_) = field.kind() {
                let consumed_semicolon = self.try_consume_before_whitespace(":").is_ok();
                self.try_consume_whitespace(descriptor.full_name(), "Normal")
                    .ok();
                let options = &field.field_descriptor_proto().options;
                let is_weak = options.as_ref().map(|opt| opt.weak()).unwrap_or(false);
                if consumed_semicolon && is_weak && self.looking_at_type(TokenType::String) {
                    let tmp = self.consume_string()?;
                    field
                        .mut_value_of(message)
                        .as_message_mut()
                        .unwrap()
                        .merge(tmp.as_bytes())
                        .map_err(|e| ProtoError::Todo(e.to_string()))?;

                    return Ok(());
                }
            } else {
                self.consume_before_whitespace(":")?;
                self.try_consume_whitespace(descriptor.full_name(), "Normal")
                    .ok();
            }

            if field.is_list() && self.try_consume("[").is_ok() {
                if self.try_consume("]").is_err() {
                    loop {
                        if let Kind::Message(_) = field.kind() {
                            self.consume_field_message(message, &field)?;
                        } else {
                            self.consume_field_value(message, &field)?;
                        }

                        if self.try_consume("]").is_ok() {
                            break;
                        }
                        self.consume(",")?
                    }
                }
            } else if let Kind::Message(_) = field.kind() {
                self.consume_field_message(message, &field)?;
            } else {
                self.consume_field_value(message, &field)?;
            }
            Ok(())
        })()?;

        let _ = self.try_consume(";").is_ok() || self.try_consume(",").is_ok();

        if field
            .field_descriptor_proto()
            .options
            .as_ref()
            .map(|op| op.deprecated())
            .unwrap_or(false)
        {
            self.report_warning(&format!(
                "text format contains deprecated field \"{}\"",
                field_name
            ));
        }

        if let Some(parse_info_tree) = &self.parse_info_tree {
            let end_line = self.tokenizer.previous().line;
            let end_column = self.tokenizer.previous().end_column;

            parse_info_tree.borrow_mut().record_location(
                &field,
                ParseLocationRange {
                    start: ParseLocation {
                        line: start_line,
                        column: start_column,
                    },
                    end: ParseLocation {
                        line: end_line,
                        column: end_column,
                    },
                },
            );
        }

        Ok(())
    }

    fn skip_field(&mut self) -> Result<()> {
        if self.try_consume("[").is_ok() {
            self.consume_type_url_or_full_type_name()?;
            self.consume_before_whitespace("]")?;
        } else {
            self.consume_identifier_before_whitespace()?;
        }

        self.try_consume_whitespace("Unkown/Reserved", "n/a").ok();

        if self.try_consume_before_whitespace(":").is_ok() {
            self.try_consume_whitespace("Unknown/Reserved", "n/a").ok();
            if !self.looking_at("{") & !self.looking_at("<") {
                self.skip_field_value()?;
            } else {
                self.skip_field_message()?;
            }
        } else {
            self.skip_field_message()?;
        }

        let _ = self.try_consume(";").is_ok() || self.try_consume(",").is_ok();
        Ok(())
    }

    fn consume_field_message(
        &mut self,
        message: &mut DynamicMessage,
        field: &FieldExtensionDescriptor,
    ) -> Result<()> {
        self.decrease_recursion_limit()?;

        let parent;
        if let Some(p) = self.parse_info_tree.as_mut() {
            let child = p.borrow_mut().create_nested(field);
            parent = Some(Rc::clone(p));
            *p = child;
        } else {
            parent = None;
        }

        let delimiter = self.consume_message_delimiter()?;
        if field.is_list() {
            let list = field
                .mut_value_of(message)
                .as_list_mut()
                .ok_or_else(|| ProtoError::Todo("".into()))?;
            if let Kind::Message(m) = field.kind() {
                list.push(Value::Message(DynamicMessage::new(m)));
            } else {
                return Err(ProtoError::Todo("Field type is not message".into()));
            }
            self.consume_message(
                list.last_mut().unwrap().as_message_mut().unwrap(),
                &delimiter,
            )?;
        } else {
            let mut inner = if let Kind::Message(m) = field.kind() {
                DynamicMessage::new(m)
            } else {
                return Err(ProtoError::Todo("".into()));
            };
            self.consume_message(&mut inner, &delimiter)?;
            field.set_into(Value::Message(inner), message);
        }

        self.opts.recursion_limit += 1;
        self.parse_info_tree = parent;

        Ok(())
    }

    fn skip_field_message(&mut self) -> Result<()> {
        self.decrease_recursion_limit()?;
        let delimiter = self.consume_message_delimiter()?;
        while !self.looking_at(">") && !self.looking_at("}") {
            self.skip_field()?;
        }
        self.consume(&delimiter)?;

        self.opts.recursion_limit += 1;
        Ok(())
    }

    fn consume_field_value(
        &mut self,
        message: &mut DynamicMessage,
        field: &FieldExtensionDescriptor,
    ) -> Result<()> {
        match field.kind() {
            Kind::Int32 | Kind::Sint32 | Kind::Sfixed32 => {
                let value = self.consume_signed_integer(i32::MAX as u64)?;
                message.set_int32(value as i32, field)?;
            }
            Kind::Uint32 | Kind::Fixed32 => {
                let value = self.consume_unsigned_integer(u32::MAX as u64)?;
                message.set_uint32(value as u32, field)?;
            }
            Kind::Int64 | Kind::Sint64 | Kind::Sfixed64 => {
                let value = self.consume_signed_integer(i64::MAX as u64)?;
                message.set_int64(value, field)?;
            }
            Kind::Uint64 | Kind::Fixed64 => {
                let value = self.consume_unsigned_integer(u64::MAX)?;
                message.set_uint64(value, field)?;
            }
            Kind::Float => {
                let value = self.consume_double()?;
                message.set_float(value as f32, field)?;
            }
            Kind::Double => {
                let value = self.consume_double()?;
                message.set_double(value, field)?;
            }
            Kind::String => {
                let value = self.consume_string()?;
                message.set_string(value, field)?;
            }
            Kind::Bytes => {
                let value = self.consume_string()?;
                message.set_bytes(value, field)?;
            }
            Kind::Bool => {
                if self.looking_at_type(TokenType::Integer) {
                    let value = self.consume_unsigned_integer(1)?;
                    message.set_bool(value == 1, field)?;
                } else {
                    let value = self.consume_identifier()?;
                    if value == "true" || value == "True" || value == "t" {
                        message.set_bool(true, field)?;
                    } else if value == "false" || value == "False" || value == "f" {
                        message.set_bool(false, field)?;
                    } else {
                        self.report_error(&format!(
                            "Invalid value for boolean field \"{}\". Value: \"{}\".",
                            field.name(),
                            &value,
                        ));
                        return Err(ProtoError::Todo("Invalid value for bool".into()));
                    }
                }
            }
            Kind::Enum(enum_descriptor) => {
                let mut int_value = None;
                let enum_value;
                let value: String;
                if self.looking_at_type(TokenType::Ident) {
                    value = self.consume_identifier()?;
                    enum_value = enum_descriptor.get_value_by_name(&value);
                } else if self.looking_at("-") || self.looking_at_type(TokenType::Integer) {
                    let v = self.consume_signed_integer(i32::MAX as u64)? as i32;
                    enum_value = enum_descriptor.get_value(v);
                    value = v.to_string();
                    int_value = Some(v);
                } else {
                    self.report_error(&format!(
                        "Expected integer or identifier, got: {}",
                        String::from_utf8_lossy(&self.tokenizer.current().text),
                    ));
                    return Err(ProtoError::Todo("Invalid value in enum".into()));
                }

                if enum_value.is_none() {
                    if int_value.is_some() && message.supports_unknown_enum() {
                        message.set_enum(int_value.unwrap(), field)?;
                    } else if !self.opts.allow_unknown_enum {
                        self.report_error(&format!(
                            "Unknown enumeration value of \"{}\" for field \"{}\".",
                            value,
                            field.name(),
                        ));
                        return Err(ProtoError::Todo("Unknown enum value".into()));
                    } else {
                        self.report_warning(&format!(
                            "Unknown enumeration value of \"{}\" for field \"{}\".",
                            value,
                            field.name(),
                        ));
                        return Ok(());
                    }
                }

                if let Some(n) = enum_value {
                    message.set_enum(n.number(), field)?;
                }
            }
            Kind::Message(_) => {
                unreachable!();
            }
        }
        Ok(())
    }

    fn skip_field_value(&mut self) -> Result<()> {
        self.decrease_recursion_limit()?;

        if self.looking_at_type(TokenType::String) {
            while self.looking_at_type(TokenType::String) {
                self.tokenizer.next();
            }
            self.opts.recursion_limit += 1;
            return Ok(());
        }

        if self.try_consume("[").is_ok() {
            loop {
                if !self.looking_at("{") && !self.looking_at("<") {
                    self.skip_field_value()?;
                } else {
                    self.skip_field_message()?;
                }
                if self.try_consume("]").is_ok() {
                    break;
                }
                self.consume(",")?;
            }
            self.opts.recursion_limit += 1;
            return Ok(());
        }

        let has_minus = self.try_consume("-").is_ok();
        if !self.looking_at_type(TokenType::Integer)
            && !self.looking_at_type(TokenType::Float)
            && !self.looking_at_type(TokenType::Ident)
        {
            let text = self.tokenizer.current().str()?.to_string();
            self.report_error(&format!(
                "Cannot skip field value, unexpected token: {}",
                text
            ));
            self.opts.recursion_limit += 1;
            return Err(ProtoError::Todo("Unexpected token while skipping".into()));
        }

        if has_minus && self.looking_at_type(TokenType::Ident) {
            let text = self.tokenizer.current().text.to_ascii_lowercase();
            if &text != b"inf" && &text != b"infinity" && text != b"nan" {
                self.report_error(&format!(
                    "Invalid float number: {}",
                    String::from_utf8_lossy(&text),
                ));
                self.opts.recursion_limit += 1;
                return Err(ProtoError::Todo("Invalid float number".into()));
            }
        }

        self.tokenizer.next();
        self.opts.recursion_limit += 1;
        Ok(())
    }

    fn looking_at(&self, text: &str) -> bool {
        return self.tokenizer.current().str().unwrap() == text;
    }

    fn looking_at_type(&self, token_type: TokenType) -> bool {
        return self.tokenizer.current().type_ == token_type;
    }

    fn consume_identifier(&mut self) -> Result<String> {
        if self.looking_at_type(TokenType::Ident) {
            let ret = self.tokenizer.current().str()?.to_string();
            self.tokenizer.next();
            return Ok(ret);
        }

        if (self.opts.allow_field_number
            || self.opts.allow_unknown_field
            || self.opts.allow_unknown_extension)
            && self.looking_at_type(TokenType::Integer)
        {
            let ret = self.tokenizer.current().str()?.to_string();
            self.tokenizer.next();
            return Ok(ret);
        }

        self.report_error(&format!(
            "Expected identifier, got: {}",
            String::from_utf8_lossy(&self.tokenizer.current().text),
        ));
        Err(ProtoError::Todo("Failed consume identifier".into()))
    }

    fn consume_identifier_before_whitespace(&mut self) -> Result<String> {
        self.tokenizer.set_report_whitespaces(true);
        let ret = self.consume_identifier();
        self.tokenizer.set_report_whitespaces(false);
        ret
    }

    fn consume_full_type_name(&mut self) -> Result<String> {
        let mut name = self.consume_identifier()?;
        while self.try_consume(".").is_ok() {
            let part = self.consume_identifier()?;
            name.push('.');
            name.push_str(&part);
        }
        Ok(name)
    }

    fn consume_type_url_or_full_type_name(&mut self) -> Result<()> {
        self.consume_identifier()?;
        while self.try_consume(".").is_ok() || self.try_consume("/").is_ok() {
            self.consume_identifier()?;
        }
        Ok(())
    }

    fn consume_string(&mut self) -> Result<String> {
        if !self.looking_at_type(TokenType::String) {
            self.report_error(&format!(
                "Expected string, got: {}",
                String::from_utf8_lossy(&self.tokenizer.current().text),
            ));
            return Err(ProtoError::Todo("Failed consume string".into()));
        }

        let mut text = String::new();
        while self.looking_at_type(TokenType::String) {
            parse_string_append(&self.tokenizer.current().text, &mut text);
            self.tokenizer.next();
        }
        Ok(text)
    }

    fn consume_unsigned_integer(&mut self, max_value: u64) -> Result<u64> {
        if !self.looking_at_type(TokenType::Integer) {
            self.report_error(&format!(
                "Expected integer, got: {}",
                String::from_utf8_lossy(&self.tokenizer.current().text),
            ));
            return Err(ProtoError::Todo("Faield consume integer".into()));
        }

        if let Ok(value) = parse_integer(&self.tokenizer.current().text, max_value) {
            self.tokenizer.next();
            Ok(value)
        } else {
            self.report_error(&format!(
                "Integer out of range ({})",
                self.tokenizer.current().str().unwrap()
            ));
            Err(ProtoError::Todo("".into()))
        }
    }

    fn consume_signed_integer(&mut self, mut max_value: u64) -> Result<i64> {
        let mut negative = false;
        if self.try_consume("-").is_ok() {
            negative = true;
            max_value += 1;
        }

        let value = self.consume_unsigned_integer(max_value)?;

        if negative {
            if i64::MAX as u64 + 1 == value {
                Ok(i64::MIN)
            } else {
                Ok(-(value as i64))
            }
        } else {
            Ok(value as i64)
        }
    }

    fn consume_unsigned_decimal_as_double(&mut self, _max_value: u64) -> Result<f64> {
        if !self.looking_at_type(TokenType::Integer) {
            self.report_error(&format!(
                "Expected integer, got: {}",
                self.tokenizer.current().str()?
            ));
            return Err(ProtoError::Todo("Faild to parse double".into()));
        }

        let text = self.tokenizer.current().str()?;
        if is_hex_number(text) || is_oct_number(text) {
            self.report_error(&format!(
                "Expected decimal number, got {}",
                self.tokenizer.current().str()?
            ));
            return Err(ProtoError::Todo("Faild to parse double".into()));
        }

        let value = text
            .parse::<f64>()
            .map_err(|e| ProtoError::Todo(e.to_string()))?;

        self.tokenizer.next();
        Ok(value)
    }

    fn consume_double(&mut self) -> Result<f64> {
        let mut negative = false;
        if self.try_consume("-").is_ok() {
            negative = true;
        }

        let v = if self.looking_at_type(TokenType::Integer) {
            self.consume_unsigned_decimal_as_double(u64::MAX)?
        } else if self.looking_at_type(TokenType::Float) {
            let mut text = self.tokenizer.current().str()?;
            let last = &text[text.len() - 1..text.len()];
            if last == "f" || last == "F" {
                text = &text[..text.len() - 1];
            }
            let v = text
                .parse::<f64>()
                .map_err(|e| ProtoError::Todo(e.to_string()))?;
            self.tokenizer.next();
            v
        } else if self.looking_at_type(TokenType::Ident) {
            let text = self.tokenizer.current().str()?.to_ascii_lowercase();
            let v;
            if text == "inf" || text == "infinity" {
                v = f64::INFINITY;
                self.tokenizer.next();
            } else if text == "nan" {
                v = f64::NAN;
                self.tokenizer.next();
            } else {
                self.report_error(&format!("Expected double, got: {}", text));
                return Err(ProtoError::Todo("Failed to consume double".into()));
            }
            v
        } else {
            self.report_error(&format!(
                "Expected double, got: {}",
                self.tokenizer.current().str()?,
            ));
            return Err(ProtoError::Todo("Failed to consume double".into()));
        };

        if negative {
            Ok(-v)
        } else {
            Ok(v)
        }
    }

    fn consume_any_type_url(&mut self) -> Result<(String, String)> {
        let mut prefix = self.consume_identifier()?;
        while self.try_consume(".").is_ok() {
            let url = self.consume_identifier()?;
            prefix.push('.');
            prefix.push_str(&url);
        }
        self.consume("/")?;
        prefix.push('/');
        let full_type_name = self.consume_full_type_name()?;

        Ok((full_type_name, prefix))
    }

    fn consume_any_value(&mut self, value_descriptor: &MessageDescriptor) -> Result<Vec<u8>> {
        let mut value = DynamicMessage::new(value_descriptor.clone());

        let sub_delimiter = self.consume_message_delimiter()?;
        self.consume_message(&mut value, &sub_delimiter)?;

        let serialized_value = if self.opts.allow_partial {
            value.encode_to_vec()
        } else {
            if !value.is_initialized() {
                self.report_error("Value of type \"{}\" stored in google.protobuf.Any has missing required fields");
                return Err(ProtoError::Todo("".into()));
            }
            value.encode_to_vec()
        };

        Ok(serialized_value)
    }

    fn consume(&mut self, value: &str) -> Result<()> {
        let current_value = &self.tokenizer.current().text;

        if current_value != value.as_bytes() {
            let msg = format!(
                "Expected \"{}\", found \"{}\".",
                value,
                std::str::from_utf8(current_value).unwrap()
            );
            self.report_error(&msg);
            return Err(ProtoError::Todo(msg));
        }

        self.tokenizer.next();
        Ok(())
    }

    fn consume_before_whitespace(&mut self, value: &str) -> Result<()> {
        self.tokenizer.set_report_whitespaces(true);
        let result = self.consume(value);
        self.tokenizer.set_report_whitespaces(false);
        result
    }

    fn try_consume(&mut self, value: &str) -> Result<()> {
        if self.tokenizer.current().text == value.as_bytes() {
            self.tokenizer.next();
            Ok(())
        } else {
            Err(ProtoError::Todo("".into()))
        }
    }

    fn try_consume_before_whitespace(&mut self, value: &str) -> Result<()> {
        self.tokenizer.set_report_whitespaces(true);
        let result = self.try_consume(value);
        self.tokenizer.set_report_whitespaces(false);
        result
    }

    fn try_consume_whitespace(&mut self, _message_type: &str, _field_type: &str) -> Result<()> {
        if self.looking_at_type(TokenType::Whitespace) {
            self.tokenizer.next();
            Ok(())
        } else {
            Err(ProtoError::Todo("".into()))
        }
    }
}

trait ReflectMessageExt: ReflectMessage {
    fn has_oneof(&self, oneof: &OneofDescriptor) -> bool;
    fn get_oneof_field_descriptor(&self, oneof: &OneofDescriptor) -> Option<FieldDescriptor>;
    fn set_int32(&mut self, value: i32, field: &FieldExtensionDescriptor) -> Result<()>;
    fn set_uint32(&mut self, value: u32, field: &FieldExtensionDescriptor) -> Result<()>;
    fn set_int64(&mut self, value: i64, field: &FieldExtensionDescriptor) -> Result<()>;
    fn set_uint64(&mut self, value: u64, field: &FieldExtensionDescriptor) -> Result<()>;
    fn set_float(&mut self, value: f32, field: &FieldExtensionDescriptor) -> Result<()>;
    fn set_double(&mut self, value: f64, field: &FieldExtensionDescriptor) -> Result<()>;
    fn set_bool(&mut self, value: bool, field: &FieldExtensionDescriptor) -> Result<()>;
    fn set_string(&mut self, value: String, field: &FieldExtensionDescriptor) -> Result<()>;
    fn set_bytes(&mut self, value: String, field: &FieldExtensionDescriptor) -> Result<()>;
    fn set_enum(&mut self, value: i32, field: &FieldExtensionDescriptor) -> Result<()>;
    fn supports_unknown_enum(&self) -> bool;
    fn is_initialized(&self) -> bool;
    fn find_initialization_errors(&self) -> Vec<String>;
    fn find_initialization_errors_rec(&self, prefix: &str) -> Vec<String>;
}

impl ReflectMessageExt for DynamicMessage {
    fn has_oneof(&self, oneof: &OneofDescriptor) -> bool {
        for field in self.descriptor().fields() {
            if let Some(o) = field.containing_oneof() {
                if o.full_name() == oneof.full_name() && self.has_field(&field) {
                    return true;
                }
            }
        }

        false
    }
    fn get_oneof_field_descriptor(&self, oneof: &OneofDescriptor) -> Option<FieldDescriptor> {
        for field in self.descriptor().fields() {
            if let Some(o) = field.containing_oneof() {
                if o.full_name() == oneof.full_name() {
                    return Some(field);
                }
            }
        }

        None
    }
    fn set_int32(&mut self, value: i32, field: &FieldExtensionDescriptor) -> Result<()> {
        if field.is_list() {
            field
                .mut_value_of(self)
                .as_list_mut()
                .unwrap()
                .push(Value::I32(value));
        } else {
            field.set_into(Value::I32(value), self);
        }
        Ok(())
    }
    fn set_uint32(&mut self, value: u32, field: &FieldExtensionDescriptor) -> Result<()> {
        if field.is_list() {
            field
                .mut_value_of(self)
                .as_list_mut()
                .unwrap()
                .push(Value::U32(value));
        } else {
            field.set_into(Value::U32(value), self);
        }
        Ok(())
    }
    fn set_int64(&mut self, value: i64, field: &FieldExtensionDescriptor) -> Result<()> {
        if field.is_list() {
            field
                .mut_value_of(self)
                .as_list_mut()
                .unwrap()
                .push(Value::I64(value));
        } else {
            field.set_into(Value::I64(value), self);
        }
        Ok(())
    }
    fn set_uint64(&mut self, value: u64, field: &FieldExtensionDescriptor) -> Result<()> {
        if field.is_list() {
            field
                .mut_value_of(self)
                .as_list_mut()
                .unwrap()
                .push(Value::U64(value));
        } else {
            field.set_into(Value::U64(value), self);
        }
        Ok(())
    }
    fn set_float(&mut self, value: f32, field: &FieldExtensionDescriptor) -> Result<()> {
        if field.is_list() {
            field
                .mut_value_of(self)
                .as_list_mut()
                .unwrap()
                .push(Value::F32(value));
        } else {
            field.set_into(Value::F32(value), self);
        }
        Ok(())
    }
    fn set_double(&mut self, value: f64, field: &FieldExtensionDescriptor) -> Result<()> {
        if field.is_list() {
            field
                .mut_value_of(self)
                .as_list_mut()
                .unwrap()
                .push(Value::F64(value));
        } else {
            field.set_into(Value::F64(value), self);
        }
        Ok(())
    }
    fn set_bool(&mut self, value: bool, field: &FieldExtensionDescriptor) -> Result<()> {
        if field.is_list() {
            field
                .mut_value_of(self)
                .as_list_mut()
                .unwrap()
                .push(Value::Bool(value));
        } else {
            field.set_into(Value::Bool(value), self);
        }
        Ok(())
    }
    fn set_string(&mut self, value: String, field: &FieldExtensionDescriptor) -> Result<()> {
        if field.is_list() {
            field
                .mut_value_of(self)
                .as_list_mut()
                .unwrap()
                .push(Value::String(value));
        } else {
            field.set_into(Value::String(value), self);
        }
        Ok(())
    }
    fn set_bytes(&mut self, value: String, field: &FieldExtensionDescriptor) -> Result<()> {
        let data = value.into_bytes().into();
        if field.is_list() {
            field
                .mut_value_of(self)
                .as_list_mut()
                .unwrap()
                .push(Value::Bytes(data));
        } else {
            field.set_into(Value::Bytes(data), self);
        }
        Ok(())
    }
    fn set_enum(&mut self, value: i32, field: &FieldExtensionDescriptor) -> Result<()> {
        if field.is_list() {
            field
                .mut_value_of(self)
                .as_list_mut()
                .unwrap()
                .push(Value::EnumNumber(value));
        } else {
            field.set_into(Value::EnumNumber(value), self);
        }
        Ok(())
    }

    fn supports_unknown_enum(&self) -> bool {
        self.descriptor()
            .parent_file_descriptor_proto()
            .syntax
            .as_ref()
            .map(|s| s == "proto3")
            .unwrap_or(false)
    }

    fn is_initialized(&self) -> bool {
        let descriptor = self.descriptor();

        for field in descriptor.fields() {
            if field.is_required() && !self.has_field(&field) {
                return false;
            }
        }

        for field in descriptor.fields() {
            if let Some(message) = field.kind().as_message() {
                if message.is_map_entry() {
                    let value_field = message.map_entry_value_field();
                    if value_field.kind().as_message().is_some() {
                        todo!();
                    }
                } else if field.is_list() {
                    for value in self.get_field(&field).as_list().unwrap() {
                        if let Some(message) = value.as_message() {
                            if !message.is_initialized() {
                                return false;
                            }
                        }
                    }
                } else if self.has_field(&field) {
                    if let Some(message) = self.get_field(&field).as_message() {
                        if !message.is_initialized() {
                            return false;
                        }
                    }
                }
            }
        }

        true
    }

    fn find_initialization_errors(&self) -> Vec<String> {
        self.find_initialization_errors_rec("")
    }

    fn find_initialization_errors_rec(&self, prefix: &str) -> Vec<String> {
        fn sub_message_prefix(
            prefix: &str,
            field: &FieldDescriptor,
            index: Option<usize>,
        ) -> String {
            let mut result: String = prefix.into();
            result.push_str(field.name());
            if let Some(index) = index {
                result.push('[');
                result.push_str(&index.to_string());
                result.push(']');
            }
            result.push('.');
            result
        }

        let mut ret = vec![];
        let descriptor = self.descriptor();

        for field in descriptor.fields() {
            if field.is_required() && !self.has_field(&field) {
                ret.push(format!("{}{}", prefix, field.name()));
            }

            if field.kind().as_message().is_some() {
                if field.is_list() {
                    for (j, value) in self.get_field(&field).as_list().unwrap().iter().enumerate() {
                        let sub_message = value.as_message().unwrap();
                        ret.extend(
                            sub_message
                                .find_initialization_errors_rec(&sub_message_prefix(
                                    prefix,
                                    &field,
                                    Some(j),
                                ))
                                .into_iter(),
                        );
                    }
                } else {
                    let field_value = self.get_field(&field);
                    let sub_message = field_value.as_message().unwrap();
                    ret.extend(
                        sub_message
                            .find_initialization_errors_rec(&sub_message_prefix(
                                prefix, &field, None,
                            ))
                            .into_iter(),
                    );
                }
            }
        }

        ret
    }
}

trait DescriptorExt {
    fn is_extension_number(&self, number: u32) -> bool;
    fn is_reserved_name(&self, name: &str) -> bool;
    fn is_reserverd_field(&self, field_number: u32) -> bool;
    fn get_field_by_lower_case_name(&self, lower_case_name: &str) -> Option<FieldDescriptor>;
}

impl DescriptorExt for MessageDescriptor {
    fn is_extension_number(&self, number: u32) -> bool {
        for rng in self.extension_ranges() {
            if rng.contains(&number) {
                return true;
            }
        }

        false
    }

    fn is_reserved_name(&self, name: &str) -> bool {
        self.reserved_names()
            .any(|reserved_name| name == reserved_name)
    }

    fn is_reserverd_field(&self, field_number: u32) -> bool {
        for rng in self.reserved_ranges() {
            if rng.contains(&field_number) {
                return true;
            }
        }

        false
    }

    fn get_field_by_lower_case_name(&self, lower_case_name: &str) -> Option<FieldDescriptor> {
        for field in self.fields() {
            if field.name().to_ascii_lowercase() == lower_case_name {
                return Some(field);
            }
        }
        None
    }
}

trait FileDescriptorExt {
    fn get_extension_by_printable_name(
        &self,
        descriptor: &MessageDescriptor,
        name: &str,
    ) -> Option<FieldDescriptor>;

    fn get_extension_by_number(
        &self,
        descriptor: &MessageDescriptor,
        number: u32,
    ) -> Option<FieldDescriptor>;
}

impl FileDescriptorExt for FileDescriptor {
    fn get_extension_by_printable_name(
        &self,
        _descriptor: &MessageDescriptor,
        _name: &str,
    ) -> Option<FieldDescriptor> {
        todo!()
    }

    fn get_extension_by_number(
        &self,
        _descriptor: &MessageDescriptor,
        _number: u32,
    ) -> Option<FieldDescriptor> {
        todo!()
    }
}

fn get_any_field_descriptors(
    message: &DynamicMessage,
) -> Option<(FieldDescriptor, FieldDescriptor)> {
    let descriptor = message.descriptor();
    if descriptor.full_name() != "google.protobuf.Any" {
        return None;
    }
    let type_url_field = descriptor.get_field(1);
    let value_field = descriptor.get_field(2);
    if let (Some(url), Some(value)) = (type_url_field, value_field) {
        Some((url, value))
    } else {
        None
    }
}

fn is_hex_number(text: &str) -> bool {
    text.starts_with("0x") || text.starts_with("0X")
}

fn is_oct_number(text: &str) -> bool {
    text.starts_with('0') && (text.chars().nth(1).map(|x| ('0'..'8').contains(&x))).unwrap_or(false)
}

trait FieldDescriptorExt {
    fn is_required(&self) -> bool;
}

impl FieldDescriptorExt for FieldDescriptor {
    fn is_required(&self) -> bool {
        let proto = self.field_descriptor_proto();
        proto.label() == prost_types::field_descriptor_proto::Label::Required
    }
}

pub fn parse<R: Read>(input: R, output: &mut DynamicMessage) -> bool {
    let parser = Parser::new();
    parser.parse(input, output)
}

pub fn parse_from_str(input: &str, output: &mut DynamicMessage) -> bool {
    parse(input.as_bytes(), output)
}

#[cfg(test)]
mod tests {
    use once_cell::sync::Lazy;
    use paste::paste;
    use prost_types::Any;
    use std::io::Cursor;

    use self::protobuf_unittest::{TestAllExtensions, TestAllTypes};

    use super::*;

    macro_rules! descriptor {
        ($file_descriptor:path, $message_type:ident, $name:expr) => {
            impl $message_type {
                pub fn descriptor() -> ::prost_reflect::MessageDescriptor {
                    $file_descriptor.get_message_by_name($name).unwrap()
                }
            }
        };
    }

    #[allow(clippy::all)]
    mod protobuf_unittest_import {
        include!(concat!(env!("OUT_DIR"), "/protobuf_unittest_import.rs"));
    }

    #[allow(clippy::all)]
    mod protobuf_unittest {
        include!(concat!(env!("OUT_DIR"), "/protobuf_unittest.rs"));
        descriptor!(
            super::FILE_DESCRIPTOR,
            TestAllTypes,
            "protobuf_unittest.TestAllTypes"
        );
        descriptor!(
            super::FILE_DESCRIPTOR,
            TestAllExtensions,
            "protobuf_unittest.TestAllExtensions"
        );
        descriptor!(
            super::FILE_DESCRIPTOR,
            SparseEnumMessage,
            "protobuf_unittest.SparseEnumMessage"
        );
        descriptor!(
            super::FILE_DESCRIPTOR,
            TestRequired,
            "protobuf_unittest.TestRequired"
        );
        descriptor!(
            super::FILE_DESCRIPTOR,
            ForeignMessage,
            "protobuf_unittest.ForeignMessage"
        );
        descriptor!(
            super::FILE_DESCRIPTOR,
            TestDeprecatedFields,
            "protobuf_unittest.TestDeprecatedFields"
        );
        descriptor!(
            super::FILE_DESCRIPTOR,
            NestedTestAllTypes,
            "protobuf_unittest.NestedTestAllTypes"
        );
        descriptor!(
            super::FILE_DESCRIPTOR,
            TestMessageSetContainer,
            "protobuf_unittest.TestMessageSetContainer"
        );
    }

    #[allow(clippy::all)]
    mod proto2_wireformat_unittest {
        include!(concat!(env!("OUT_DIR"), "/proto2_wireformat_unittest.rs"));
    }

    #[allow(clippy::all)]
    mod proto3_unittest {
        include!(concat!(env!("OUT_DIR"), "/proto3_unittest.rs"));
        descriptor!(
            super::FILE_DESCRIPTOR,
            TestAllTypes,
            "proto3_unittest.TestAllTypes"
        );
    }

    static FILE_DESCRIPTOR: Lazy<FileDescriptor> = Lazy::new(|| {
        FileDescriptor::new(
            prost_types::FileDescriptorSet::decode(
                include_bytes!(concat!(env!("OUT_DIR"), "/file_descriptor_set_test.bin")).as_ref(),
            )
            .unwrap(),
        )
        .unwrap()
    });

    #[derive(Debug, Default)]
    struct MockErrorCollctor {
        text: String,
    }

    impl ErrorCollector for MockErrorCollctor {
        fn add_error(&mut self, line: usize, column: usize, msg: &str) {
            self.text
                .push_str(&format!("{}:{}: {}\n", line + 1, column + 1, msg));
        }
        fn add_warning(&mut self, line: usize, column: usize, msg: &str) {
            self.add_error(line, column, &format!("WARNING:{}", msg));
        }
    }

    fn expect_success_and_tree(
        parser: &mut Parser,
        text: &str,
        message: &mut DynamicMessage,
        info_tree: Rc<RefCell<ParseInfoTree>>,
    ) {
        let error_collector = Rc::new(RefCell::new(MockErrorCollctor::default()));
        parser.record_error_to(Rc::clone(&error_collector) as Rc<RefCell<dyn ErrorCollector>>);
        parser.write_localtions_to(Rc::clone(&info_tree));
        assert!(parser.parse_from_str(text, message));
    }

    fn expect_location(
        tree: &ParseInfoTree,
        d: &MessageDescriptor,
        field_name: &str,
        index: usize,
        expected_range: ParseLocationRange,
    ) {
        let range = tree
            .get_location_range(&d.get_field_by_name(field_name).unwrap(), index)
            .unwrap();
        assert_eq!(&expected_range, range);
        let start_location = tree
            .get_location(&d.get_field_by_name(field_name).unwrap(), index)
            .unwrap();
        assert_eq!(&expected_range.start, start_location);
    }

    fn expect_location_not_found(
        tree: &ParseInfoTree,
        d: &MessageDescriptor,
        field_name: &str,
        index: usize,
    ) {
        assert!(&d
            .get_field_by_name(field_name)
            .and_then(|f| tree.get_location_range(&f, index))
            .is_none());
    }

    fn expect_failure(input: &str, message: &str, line: usize, col: usize) {
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        expected_failure_from(input, message, line, col, &mut proto);
    }

    fn expected_failure_from(
        input: &str,
        message: &str,
        line: usize,
        col: usize,
        proto: &mut DynamicMessage,
    ) {
        let mut parser = Parser::new();
        expect_message(&mut parser, input, message, line, col, proto, false);
    }

    fn expect_message(
        parser: &mut Parser,
        input: &str,
        message: &str,
        line: usize,
        col: usize,
        proto: &mut DynamicMessage,
        expected_result: bool,
    ) {
        let error_collector = Rc::new(RefCell::new(MockErrorCollctor::default()));
        parser.record_error_to(Rc::clone(&error_collector) as Rc<RefCell<dyn ErrorCollector>>);
        assert_eq!(parser.parse_from_str(input, proto), expected_result);
        assert_eq!(
            format!("{}:{}: {}\n", line, col, message),
            error_collector.borrow().text
        );
    }

    #[allow(clippy::bool_assert_comparison)]
    fn assert_all_fields_set(message: &DynamicMessage) {
        let message = message.transcode_to::<TestAllTypes>().unwrap();

        assert_eq!(101, message.optional_int32());
        assert_eq!(102, message.optional_int64());
        assert_eq!(103, message.optional_uint32());
        assert_eq!(104, message.optional_uint64());
        assert_eq!(105, message.optional_sint32());
        assert_eq!(106, message.optional_sint64());
        assert_eq!(107, message.optional_fixed32());
        assert_eq!(108, message.optional_fixed64());
        assert_eq!(109, message.optional_sfixed32());
        assert_eq!(110, message.optional_sfixed64());
        assert_eq!(111.0, message.optional_float());
        assert_eq!(112.0, message.optional_double());
        assert_eq!(true, message.optional_bool());
        assert_eq!("115", message.optional_string());
        assert_eq!(b"116", message.optional_bytes());

        assert_eq!(117, message.optionalgroup.as_ref().unwrap().a());
        assert_eq!(118, message.optional_nested_message.as_ref().unwrap().bb());
        assert_eq!(119, message.optional_foreign_message.as_ref().unwrap().c());
        assert_eq!(120, message.optional_import_message.as_ref().unwrap().d());
        assert_eq!(
            126,
            message.optional_public_import_message.as_ref().unwrap().e()
        );
        assert_eq!(127, message.optional_lazy_message.as_ref().unwrap().bb());

        assert_eq!(
            protobuf_unittest::test_all_types::NestedEnum::Baz,
            message.optional_nested_enum()
        );
        assert_eq!(
            protobuf_unittest::ForeignEnum::ForeignBaz,
            message.optional_foreign_enum()
        );
        assert_eq!(
            protobuf_unittest_import::ImportEnum::ImportBaz,
            message.optional_import_enum()
        );

        assert_eq!([201, 301].as_ref(), &message.repeated_int32);
        assert_eq!([202, 302].as_ref(), &message.repeated_int64);
        assert_eq!([203, 303].as_ref(), &message.repeated_uint32);
        assert_eq!([204, 304].as_ref(), &message.repeated_uint64);
        assert_eq!([205, 305].as_ref(), &message.repeated_sint32);
        assert_eq!([206, 306].as_ref(), &message.repeated_sint64);
        assert_eq!([207, 307].as_ref(), &message.repeated_fixed32);
        assert_eq!([208, 308].as_ref(), &message.repeated_fixed64);
        assert_eq!([209, 309].as_ref(), &message.repeated_sfixed32);
        assert_eq!([210, 310].as_ref(), &message.repeated_sfixed64);
        assert_eq!([211.0, 311.0].as_ref(), &message.repeated_float);
        assert_eq!([212.0, 312.0].as_ref(), &message.repeated_double);
        assert_eq!([true, false].as_ref(), &message.repeated_bool);
        assert_eq!(["215", "315"].as_ref(), &message.repeated_string);
        assert_eq!(
            [b"216".to_vec(), b"316".to_vec()].as_ref(),
            &message.repeated_bytes
        );
        assert_eq!(217, message.repeatedgroup[0].a());
        assert_eq!(317, message.repeatedgroup[1].a());
        assert_eq!(218, message.repeated_nested_message[0].bb());
        assert_eq!(318, message.repeated_nested_message[1].bb());
        assert_eq!(219, message.repeated_foreign_message[0].c());
        assert_eq!(319, message.repeated_foreign_message[1].c());
        assert_eq!(220, message.repeated_import_message[0].d());
        assert_eq!(320, message.repeated_import_message[1].d());
        assert_eq!(227, message.repeated_lazy_message[0].bb());
        assert_eq!(327, message.repeated_lazy_message[1].bb());

        assert_eq!(
            protobuf_unittest::test_all_types::NestedEnum::Bar,
            message.repeated_nested_enum().next().unwrap()
        );
        assert_eq!(
            protobuf_unittest::test_all_types::NestedEnum::Baz,
            message.repeated_nested_enum().nth(1).unwrap()
        );
        assert_eq!(
            protobuf_unittest::ForeignEnum::ForeignBar,
            message.repeated_foreign_enum().next().unwrap()
        );
        assert_eq!(
            protobuf_unittest::ForeignEnum::ForeignBaz,
            message.repeated_foreign_enum().nth(1).unwrap()
        );
        assert_eq!(
            protobuf_unittest_import::ImportEnum::ImportBar,
            message.repeated_import_enum().next().unwrap()
        );
        assert_eq!(
            protobuf_unittest_import::ImportEnum::ImportBaz,
            message.repeated_import_enum().nth(1).unwrap()
        );

        assert_eq!(401, message.default_int32());
        assert_eq!(402, message.default_int64());
        assert_eq!(403, message.default_uint32());
        assert_eq!(404, message.default_uint64());
        assert_eq!(405, message.default_sint32());
        assert_eq!(406, message.default_sint64());
        assert_eq!(407, message.default_fixed32());
        assert_eq!(408, message.default_fixed64());
        assert_eq!(409, message.default_sfixed32());
        assert_eq!(410, message.default_sfixed64());
        assert_eq!(411.0, message.default_float());
        assert_eq!(412.0, message.default_double());
        assert_eq!(false, message.default_bool());
        assert_eq!("415", message.default_string());
        assert_eq!(b"416", message.default_bytes());

        assert_eq!(
            protobuf_unittest::test_all_types::NestedEnum::Foo,
            message.default_nested_enum()
        );
        assert_eq!(
            protobuf_unittest::ForeignEnum::ForeignFoo,
            message.default_foreign_enum()
        );
        assert_eq!(
            protobuf_unittest_import::ImportEnum::ImportFoo,
            message.default_import_enum()
        );

        assert_eq!(
            protobuf_unittest::test_all_types::OneofField::OneofBytes(b"604".to_vec()),
            message.oneof_field.unwrap()
        );
    }

    #[allow(clippy::bool_assert_comparison)]
    fn assert_all_extensions_set(message: &DynamicMessage) {
        macro_rules! ext {
            ($extension_name:ident) => {{
                let desc = message.descriptor();
                desc.get_extension_by_json_name(concat!(
                    "[protobuf_unittest.",
                    stringify!($extension_name),
                    "]"
                ))
                .as_ref()
                .unwrap()
            }};
        }
        assert!(message.has_extension(ext!(optional_int32_extension)));
        assert!(message.has_extension(ext!(optional_int64_extension)));
        assert!(message.has_extension(ext!(optional_uint32_extension)));
        assert!(message.has_extension(ext!(optional_uint64_extension)));
        assert!(message.has_extension(ext!(optional_sint32_extension)));
        assert!(message.has_extension(ext!(optional_sint64_extension)));
        assert!(message.has_extension(ext!(optional_fixed32_extension)));
        assert!(message.has_extension(ext!(optional_fixed64_extension)));
        assert!(message.has_extension(ext!(optional_sfixed32_extension)));
        assert!(message.has_extension(ext!(optional_sfixed64_extension)));
        assert!(message.has_extension(ext!(optional_float_extension)));
        assert!(message.has_extension(ext!(optional_double_extension)));
        assert!(message.has_extension(ext!(optional_bool_extension)));
        assert!(message.has_extension(ext!(optional_string_extension)));
        assert!(message.has_extension(ext!(optional_bytes_extension)));
        assert!(message.has_extension(ext!(optionalgroup_extension)));
        assert!(message.has_extension(ext!(optional_nested_message_extension)));
        assert!(message.has_extension(ext!(optional_foreign_message_extension)));
        assert!(message.has_extension(ext!(optional_import_message_extension)));
        assert!(message.has_extension(ext!(optional_public_import_message_extension)));
        assert!(message.has_extension(ext!(optional_lazy_message_extension)));

        assert!(message
            .get_extension(ext!(optionalgroup_extension))
            .as_message()
            .unwrap()
            .has_field_by_name("a"));
        assert!(message
            .get_extension(ext!(optional_nested_message_extension))
            .as_message()
            .unwrap()
            .has_field_by_name("bb"));
        assert!(message
            .get_extension(ext!(optional_foreign_message_extension))
            .as_message()
            .unwrap()
            .has_field_by_name("c"));
        assert!(message
            .get_extension(ext!(optional_import_message_extension))
            .as_message()
            .unwrap()
            .has_field_by_name("d"));
        assert!(message
            .get_extension(ext!(optional_public_import_message_extension))
            .as_message()
            .unwrap()
            .has_field_by_name("e"));
        assert!(message
            .get_extension(ext!(optional_lazy_message_extension))
            .as_message()
            .unwrap()
            .has_field_by_name("bb"));

        assert_eq!(
            101,
            message
                .get_extension(ext!(optional_int32_extension))
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            102,
            message
                .get_extension(ext!(optional_int64_extension))
                .as_i64()
                .unwrap()
        );
        assert_eq!(
            103,
            message
                .get_extension(ext!(optional_uint32_extension))
                .as_u32()
                .unwrap()
        );
        assert_eq!(
            104,
            message
                .get_extension(ext!(optional_uint64_extension))
                .as_u64()
                .unwrap()
        );
        assert_eq!(
            105,
            message
                .get_extension(ext!(optional_sint32_extension))
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            106,
            message
                .get_extension(ext!(optional_sint64_extension))
                .as_i64()
                .unwrap()
        );
        assert_eq!(
            107,
            message
                .get_extension(ext!(optional_fixed32_extension))
                .as_u32()
                .unwrap()
        );
        assert_eq!(
            108,
            message
                .get_extension(ext!(optional_fixed64_extension))
                .as_u64()
                .unwrap()
        );
        assert_eq!(
            109,
            message
                .get_extension(ext!(optional_sfixed32_extension))
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            110,
            message
                .get_extension(ext!(optional_sfixed64_extension))
                .as_i64()
                .unwrap()
        );
        assert_eq!(
            111.0,
            message
                .get_extension(ext!(optional_float_extension))
                .as_f32()
                .unwrap()
        );
        assert_eq!(
            112.0,
            message
                .get_extension(ext!(optional_double_extension))
                .as_f64()
                .unwrap()
        );
        assert_eq!(
            true,
            message
                .get_extension(ext!(optional_bool_extension))
                .as_bool()
                .unwrap()
        );
        assert_eq!(
            "115",
            message
                .get_extension(ext!(optional_string_extension))
                .as_str()
                .unwrap()
        );
        assert_eq!(
            b"116",
            message
                .get_extension(ext!(optional_bytes_extension))
                .as_bytes()
                .unwrap()
                .as_ref()
        );

        assert_eq!(
            117,
            message
                .get_extension(ext!(optionalgroup_extension))
                .as_message()
                .unwrap()
                .get_field_by_name("a")
                .unwrap()
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            118,
            message
                .get_extension(ext!(optional_nested_message_extension))
                .as_message()
                .unwrap()
                .get_field_by_name("bb")
                .unwrap()
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            119,
            message
                .get_extension(ext!(optional_foreign_message_extension))
                .as_message()
                .unwrap()
                .get_field_by_name("c")
                .unwrap()
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            120,
            message
                .get_extension(ext!(optional_import_message_extension))
                .as_message()
                .unwrap()
                .get_field_by_name("d")
                .unwrap()
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            126,
            message
                .get_extension(ext!(optional_public_import_message_extension))
                .as_message()
                .unwrap()
                .get_field_by_name("e")
                .unwrap()
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            127,
            message
                .get_extension(ext!(optional_lazy_message_extension))
                .as_message()
                .unwrap()
                .get_field_by_name("bb")
                .unwrap()
                .as_i32()
                .unwrap()
        );

        assert_eq!(
            protobuf_unittest::test_all_types::NestedEnum::Baz as i32,
            message
                .get_extension(ext!(optional_nested_enum_extension))
                .as_enum_number()
                .unwrap()
        );
        assert_eq!(
            protobuf_unittest::ForeignEnum::ForeignBaz as i32,
            message
                .get_extension(ext!(optional_foreign_enum_extension))
                .as_enum_number()
                .unwrap()
        );
        assert_eq!(
            protobuf_unittest_import::ImportEnum::ImportBaz as i32,
            message
                .get_extension(ext!(optional_import_enum_extension))
                .as_enum_number()
                .unwrap()
        );

        macro_rules! to_vec {
            ($ext:ident, $conv:expr) => {{
                message
                    .get_extension(ext!($ext))
                    .as_list()
                    .unwrap()
                    .iter()
                    .map($conv)
                    .collect::<Vec<_>>()
            }};
        }

        assert_eq!(
            [201, 301].as_ref(),
            to_vec!(repeated_int32_extension, |x| x.as_i32().unwrap())
        );
        assert_eq!(
            [202, 302].as_ref(),
            to_vec!(repeated_int64_extension, |x| x.as_i64().unwrap())
        );
        assert_eq!(
            [203, 303].as_ref(),
            to_vec!(repeated_uint32_extension, |x| x.as_u32().unwrap())
        );
        assert_eq!(
            [204, 304].as_ref(),
            to_vec!(repeated_uint64_extension, |x| x.as_u64().unwrap())
        );
        assert_eq!(
            [205, 305].as_ref(),
            to_vec!(repeated_sint32_extension, |x| x.as_i32().unwrap())
        );
        assert_eq!(
            [206, 306].as_ref(),
            to_vec!(repeated_sint64_extension, |x| x.as_i64().unwrap())
        );
        assert_eq!(
            [207, 307].as_ref(),
            to_vec!(repeated_fixed32_extension, |x| x.as_u32().unwrap())
        );
        assert_eq!(
            [208, 308].as_ref(),
            to_vec!(repeated_fixed64_extension, |x| x.as_u64().unwrap())
        );
        assert_eq!(
            [209, 309].as_ref(),
            to_vec!(repeated_sfixed32_extension, |x| x.as_i32().unwrap())
        );
        assert_eq!(
            [210, 310].as_ref(),
            to_vec!(repeated_sfixed64_extension, |x| x.as_i64().unwrap())
        );
        assert_eq!(
            [211.0, 311.0].as_ref(),
            to_vec!(repeated_float_extension, |x| x.as_f32().unwrap())
        );
        assert_eq!(
            [212.0, 312.0].as_ref(),
            to_vec!(repeated_double_extension, |x| x.as_f64().unwrap())
        );
        assert_eq!(
            [true, false].as_ref(),
            to_vec!(repeated_bool_extension, |x| x.as_bool().unwrap())
        );
        assert_eq!(
            [String::from("215"), String::from("315")].as_ref(),
            to_vec!(repeated_string_extension, |x| x.as_str().unwrap())
        );
        assert_eq!(
            [bytes::Bytes::from(b"216".as_ref()), b"316".as_ref().into()].as_ref(),
            to_vec!(repeated_bytes_extension, |x| x.as_bytes().unwrap())
        );

        assert_eq!(
            [217, 317].as_ref(),
            to_vec!(repeatedgroup_extension, |x| x
                .as_message()
                .unwrap()
                .get_field_by_name("a")
                .unwrap()
                .as_i32()
                .unwrap())
        );
        assert_eq!(
            [218, 318].as_ref(),
            to_vec!(repeated_nested_message_extension, |x| x
                .as_message()
                .unwrap()
                .get_field_by_name("bb")
                .unwrap()
                .as_i32()
                .unwrap())
        );
        assert_eq!(
            [219, 319].as_ref(),
            to_vec!(repeated_foreign_message_extension, |x| x
                .as_message()
                .unwrap()
                .get_field_by_name("c")
                .unwrap()
                .as_i32()
                .unwrap())
        );
        assert_eq!(
            [220, 320].as_ref(),
            to_vec!(repeated_import_message_extension, |x| x
                .as_message()
                .unwrap()
                .get_field_by_name("d")
                .unwrap()
                .as_i32()
                .unwrap())
        );
        assert_eq!(
            [227, 327].as_ref(),
            to_vec!(repeated_lazy_message_extension, |x| x
                .as_message()
                .unwrap()
                .get_field_by_name("bb")
                .unwrap()
                .as_i32()
                .unwrap())
        );

        assert_eq!(
            [
                protobuf_unittest::test_all_types::NestedEnum::Bar as i32,
                protobuf_unittest::test_all_types::NestedEnum::Baz as i32,
            ]
            .as_ref(),
            to_vec!(repeated_nested_enum_extension, |x| x
                .as_enum_number()
                .unwrap())
        );
        assert_eq!(
            [
                protobuf_unittest::ForeignEnum::ForeignBar as i32,
                protobuf_unittest::ForeignEnum::ForeignBaz as i32,
            ]
            .as_ref(),
            to_vec!(repeated_foreign_enum_extension, |x| x
                .as_enum_number()
                .unwrap())
        );
        assert_eq!(
            [
                protobuf_unittest_import::ImportEnum::ImportBar as i32,
                protobuf_unittest_import::ImportEnum::ImportBaz as i32,
            ]
            .as_ref(),
            to_vec!(repeated_import_enum_extension, |x| x
                .as_enum_number()
                .unwrap())
        );

        assert_eq!(
            401,
            message
                .get_extension(ext!(default_int32_extension))
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            402,
            message
                .get_extension(ext!(default_int64_extension))
                .as_i64()
                .unwrap()
        );
        assert_eq!(
            403,
            message
                .get_extension(ext!(default_uint32_extension))
                .as_u32()
                .unwrap()
        );
        assert_eq!(
            404,
            message
                .get_extension(ext!(default_uint64_extension))
                .as_u64()
                .unwrap()
        );
        assert_eq!(
            405,
            message
                .get_extension(ext!(default_sint32_extension))
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            406,
            message
                .get_extension(ext!(default_sint64_extension))
                .as_i64()
                .unwrap()
        );
        assert_eq!(
            407,
            message
                .get_extension(ext!(default_fixed32_extension))
                .as_u32()
                .unwrap()
        );
        assert_eq!(
            408,
            message
                .get_extension(ext!(default_fixed64_extension))
                .as_u64()
                .unwrap()
        );
        assert_eq!(
            409,
            message
                .get_extension(ext!(default_sfixed32_extension))
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            410,
            message
                .get_extension(ext!(default_sfixed64_extension))
                .as_i64()
                .unwrap()
        );
        assert_eq!(
            411.0,
            message
                .get_extension(ext!(default_float_extension))
                .as_f32()
                .unwrap()
        );
        assert_eq!(
            412.0,
            message
                .get_extension(ext!(default_double_extension))
                .as_f64()
                .unwrap()
        );
        assert_eq!(
            false,
            message
                .get_extension(ext!(default_bool_extension))
                .as_bool()
                .unwrap()
        );
        assert_eq!(
            "415",
            message
                .get_extension(ext!(default_string_extension))
                .as_str()
                .unwrap()
        );
        assert_eq!(
            b"416".as_ref(),
            message
                .get_extension(ext!(default_bytes_extension))
                .as_bytes()
                .unwrap()
        );

        assert_eq!(
            protobuf_unittest::test_all_types::NestedEnum::Foo as i32,
            message
                .get_extension(ext!(default_nested_enum_extension))
                .as_enum_number()
                .unwrap()
        );
        assert_eq!(
            protobuf_unittest::ForeignEnum::ForeignFoo as i32,
            message
                .get_extension(ext!(default_foreign_enum_extension))
                .as_enum_number()
                .unwrap()
        );
        assert_eq!(
            protobuf_unittest_import::ImportEnum::ImportFoo as i32,
            message
                .get_extension(ext!(default_import_enum_extension))
                .as_enum_number()
                .unwrap()
        );

        assert_eq!(
            601,
            message
                .get_extension(ext!(oneof_uint32_extension))
                .as_u32()
                .unwrap()
        );
        assert_eq!(
            602,
            message
                .get_extension(ext!(oneof_nested_message_extension))
                .as_message()
                .unwrap()
                .get_field_by_name("bb")
                .unwrap()
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            "603",
            message
                .get_extension(ext!(oneof_string_extension))
                .as_str()
                .unwrap()
        );
        assert_eq!(
            b"604".as_ref(),
            message
                .get_extension(ext!(oneof_bytes_extension))
                .as_bytes()
                .unwrap()
        );
    }

    #[test]
    fn parse_basic() {
        let mut message = DynamicMessage::new(TestAllTypes::descriptor());
        let input_stream = Cursor::new(include_bytes!(
            "../tests/text_format_unittest_data_oneof_implemented.txt"
        ));

        assert!(parse(input_stream, &mut message));
        assert_all_fields_set(&message);
    }

    #[test]
    fn parse_extensions() {
        let mut message = DynamicMessage::new(TestAllExtensions::descriptor());
        let input_stream = Cursor::new(include_bytes!(
            "../tests/text_format_unittest_extensions_data.txt"
        ));

        assert!(parse(input_stream, &mut message));
        assert_all_extensions_set(&message);
    }

    #[test]
    fn parse_enum_field_from_number() {
        let enum_value = protobuf_unittest::test_all_types::NestedEnum::Baz;
        let parse_string = format!("optional_nested_enum: {}", enum_value as i32);
        let mut message = DynamicMessage::new(TestAllTypes::descriptor());
        assert!(parse_from_str(&parse_string, &mut message));
        assert_eq!(
            enum_value,
            message
                .transcode_to::<TestAllTypes>()
                .unwrap()
                .optional_nested_enum()
        )
    }

    #[test]
    fn parse_enum_field_from_negative_number() {
        let enum_value = protobuf_unittest::TestSparseEnum::SparseE;
        assert!((enum_value as i32) < 0);
        let parse_string = format!("sparse_enum: {}", enum_value as i32);
        let mut message = DynamicMessage::new(protobuf_unittest::SparseEnumMessage::descriptor());
        assert!(parse_from_str(&parse_string, &mut message));
        assert_eq!(
            enum_value,
            message
                .transcode_to::<protobuf_unittest::SparseEnumMessage>()
                .unwrap()
                .sparse_enum()
        );
    }

    #[test]
    fn parse_unknown_enum_field_proto3() {
        let parse_string = "repeated_nested_enum: [10, -10, 2147483647, -2147483648]";
        let mut proto = DynamicMessage::new(proto3_unittest::TestAllTypes::descriptor());
        assert!(parse_from_str(parse_string, &mut proto));
        let proto = proto.transcode_to::<TestAllTypes>().unwrap();
        assert_eq!(4, proto.repeated_nested_enum.len());
        assert_eq!(10, proto.repeated_nested_enum[0]);
        assert_eq!(-10, proto.repeated_nested_enum[1]);
        assert_eq!(2147483647, proto.repeated_nested_enum[2]);
        assert_eq!(-2147483648, proto.repeated_nested_enum[3]);
    }

    #[test]
    fn parse_string_escape() {
        let parse_string =
            "optional_string: \"\\\"A string with \\' characters \\n and \\r newlines \
            and \\t tabs and \\001 slashes \\\\ and  multiple   spaces \"";

        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        parse(parse_string.as_bytes(), &mut proto);

        let expected_string = "\"A string with ' characters \n and \r newlines and \t tabs \
            and \u{001} slashes \\ and  multiple   spaces ";

        assert_eq!(
            expected_string,
            proto
                .transcode_to::<TestAllTypes>()
                .unwrap()
                .optional_string
                .unwrap()
        );
    }

    #[test]
    fn parse_concatenated_string() {
        let parse_string = "optional_string: \"foo\" \"bar\"\n";
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        parse(parse_string.as_bytes(), &mut proto);

        assert_eq!(
            "foobar",
            proto
                .transcode_to::<TestAllTypes>()
                .unwrap()
                .optional_string
                .unwrap()
        );

        let parse_string = "optional_string: \"foo\"\n\"bar\"\n";
        parse(parse_string.as_bytes(), &mut proto);
        assert_eq!(
            "foobar",
            proto
                .transcode_to::<TestAllTypes>()
                .unwrap()
                .optional_string
                .unwrap()
        );
    }

    #[test]
    fn parse_float_with_suffix() {
        let parse_string = "optional_float: 1.0f\n";
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        parse(parse_string.as_bytes(), &mut proto);

        assert_eq!(
            1.0f32,
            proto
                .transcode_to::<TestAllTypes>()
                .unwrap()
                .optional_float
                .unwrap()
        );
    }

    #[test]
    fn parse_short_repeated_form() {
        let parse_string = "\
            repeated_int32: 1\n\
            repeated_int32: [456, 789]\n\
            repeated_nested_enum: [  FOO, BAR, # comment\n
            3]\n\
            repeated_string: [ \"foo\", 'bar' ]\n\
            repeated_nested_message: [ { bb: 1 }, { bb: 2 }]\n\
            RepeatedGroup [{ a: 3 },{ a: 4}]\n";

        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());

        assert!(parse_from_str(parse_string, &mut proto));

        let proto = proto.transcode_to::<TestAllTypes>().unwrap();
        assert_eq!(3, proto.repeated_int32.len());
        assert_eq!(1, proto.repeated_int32[0]);
        assert_eq!(456, proto.repeated_int32[1]);
        assert_eq!(789, proto.repeated_int32[2]);

        use protobuf_unittest::test_all_types::NestedEnum;
        assert_eq!(3, proto.repeated_nested_enum.len());
        assert_eq!(NestedEnum::Foo as i32, proto.repeated_nested_enum[0]);
        assert_eq!(NestedEnum::Bar as i32, proto.repeated_nested_enum[1]);
        assert_eq!(NestedEnum::Baz as i32, proto.repeated_nested_enum[2]);

        assert_eq!(2, proto.repeated_string.len());
        assert_eq!("foo", &proto.repeated_string[0]);
        assert_eq!("bar", &proto.repeated_string[1]);

        assert_eq!(2, proto.repeated_nested_message.len());
        assert_eq!(1, proto.repeated_nested_message[0].bb.unwrap());
        assert_eq!(2, proto.repeated_nested_message[1].bb.unwrap());

        assert_eq!(2, proto.repeatedgroup.len());
        assert_eq!(3, proto.repeatedgroup[0].a());
        assert_eq!(4, proto.repeatedgroup[1].a());
    }

    #[test]
    fn parse_short_repeated_with_trailing_comma() {
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        let parse_string = "repeated_int32: [456,]\n";
        assert!(!parse_from_str(parse_string, &mut proto));
        let parse_string = "repeated_nested_enum: [ FOO , ]";
        assert!(!parse_from_str(parse_string, &mut proto));
        let parse_string = "repeated_string: [ \"foo\", ]";
        assert!(!parse_from_str(parse_string, &mut proto));
        let parse_string = "repeated_nested_message: [ { bb: 1 }, ]";
        assert!(!parse_from_str(parse_string, &mut proto));
        let parse_string = "RepeatedGroup [{ a: 3 },]\n";
        assert!(!parse_from_str(parse_string, &mut proto));
    }

    #[test]
    fn parse_short_repeated_empty() {
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        let parse_string = "repeated_int32: []\n\
            repeated_nested_enum: []\n\
            repeated_string: []\n\
            repeated_nested_message: []\n\
            RepeatedGroup []\n";

        assert!(parse_from_str(parse_string, &mut proto));
        let proto = proto.transcode_to::<TestAllTypes>().unwrap();

        assert_eq!(0, proto.repeated_int32.len());
        assert_eq!(0, proto.repeated_nested_enum.len());
        assert_eq!(0, proto.repeated_string.len());
        assert_eq!(0, proto.repeated_nested_message.len());
        assert_eq!(0, proto.repeatedgroup.len());
    }

    #[test]
    fn parse_short_repeated_concatenated_with_empty() {
        let parse_string = "\
            repeated_int32: []\n\
            repeated_nested_enum: []\n\
            repeated_string: []\n\
            repeated_nested_message: []\n\
            RepeatedGroup []\n
            repeated_int32: 1\n\
            repeated_int32: [456, 789]\n\
            repeated_nested_enum: [  FOO, BAR, # comment\n
            3]\n\
            repeated_string: [ \"foo\", 'bar' ]\n\
            repeated_nested_message: [ { bb: 1 }, { bb: 2 }]\n\
            RepeatedGroup [{ a: 3 },{ a: 4}]\n\
            repeated_int32: []\n\
            repeated_nested_enum: []\n\
            repeated_string: []\n\
            repeated_nested_message: []\n\
            RepeatedGroup []\n";

        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());

        assert!(parse_from_str(parse_string, &mut proto));

        let proto = proto.transcode_to::<TestAllTypes>().unwrap();
        assert_eq!(3, proto.repeated_int32.len());
        assert_eq!(1, proto.repeated_int32[0]);
        assert_eq!(456, proto.repeated_int32[1]);
        assert_eq!(789, proto.repeated_int32[2]);

        use protobuf_unittest::test_all_types::NestedEnum;
        assert_eq!(3, proto.repeated_nested_enum.len());
        assert_eq!(NestedEnum::Foo as i32, proto.repeated_nested_enum[0]);
        assert_eq!(NestedEnum::Bar as i32, proto.repeated_nested_enum[1]);
        assert_eq!(NestedEnum::Baz as i32, proto.repeated_nested_enum[2]);

        assert_eq!(2, proto.repeated_string.len());
        assert_eq!("foo", &proto.repeated_string[0]);
        assert_eq!("bar", &proto.repeated_string[1]);

        assert_eq!(2, proto.repeated_nested_message.len());
        assert_eq!(1, proto.repeated_nested_message[0].bb.unwrap());
        assert_eq!(2, proto.repeated_nested_message[1].bb.unwrap());

        assert_eq!(2, proto.repeatedgroup.len());
        assert_eq!(3, proto.repeatedgroup[0].a());
        assert_eq!(4, proto.repeatedgroup[1].a());
    }

    #[test]
    fn parse_info_tree_building() {
        let mut message = DynamicMessage::new(TestAllTypes::descriptor());
        let string_data = "optional_int32: 1
optional_int64: 2
  optional_double: 2.4
repeated_int32: 5
repeated_int32: 10
optional_nested_message <
  bb: 78
>
repeated_nested_message <
  bb: 79
>
repeated_nested_message <
  bb: 80
>";
        let tree = ParseInfoTree::new();
        let mut parser = Parser::new();
        expect_success_and_tree(&mut parser, string_data, &mut message, Rc::clone(&tree));

        let tree = tree.borrow();
        let d = message.descriptor();
        expect_location(&tree, &d, "optional_int32", 0, (0, 0, 0, 17).into());
        expect_location(&tree, &d, "optional_int64", 0, (1, 0, 1, 17).into());
        expect_location(&tree, &d, "optional_double", 0, (2, 2, 2, 22).into());

        expect_location(&tree, &d, "repeated_int32", 0, (3, 0, 3, 17).into());
        expect_location(&tree, &d, "repeated_int32", 1, (4, 0, 4, 18).into());

        expect_location(&tree, &d, "optional_nested_message", 0, (5, 0, 7, 1).into());
        expect_location(
            &tree,
            &d,
            "repeated_nested_message",
            0,
            (8, 0, 10, 1).into(),
        );
        expect_location(
            &tree,
            &d,
            "repeated_nested_message",
            1,
            (11, 0, 13, 1).into(),
        );

        expect_location_not_found(&tree, &d, "repeated_int64", 0);
        expect_location_not_found(&tree, &d, "repeated_int32", 6);
        expect_location_not_found(&tree, &d, "some_unknown_field", 6);

        let nested_field = d.get_field_by_name("optional_nested_message").unwrap();
        let nested_tree = tree.get_tree_for_nested(&nested_field, 0).unwrap();
        expect_location(
            &nested_tree.borrow(),
            nested_field.kind().as_message().unwrap(),
            "bb",
            0,
            (6, 2, 6, 8).into(),
        );

        let nested_field = d.get_field_by_name("repeated_nested_message").unwrap();
        let nested_tree = tree.get_tree_for_nested(&nested_field, 0).unwrap();
        expect_location(
            &nested_tree.borrow(),
            nested_field.kind().as_message().unwrap(),
            "bb",
            0,
            (9, 2, 9, 8).into(),
        );

        let nested_tree = tree.get_tree_for_nested(&nested_field, 1).unwrap();
        expect_location(
            &nested_tree.borrow(),
            nested_field.kind().as_message().unwrap(),
            "bb",
            0,
            (12, 2, 12, 8).into(),
        );

        assert!(tree.get_tree_for_nested(&nested_field, 2).is_none());
    }

    #[test]
    fn comments() {
        let parse_string = "optional_int32: 1  # a comment\n\
            optional_int64: 2  # another comment";

        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        parse(parse_string.as_bytes(), &mut proto);
        let proto = proto.transcode_to::<TestAllTypes>().unwrap();

        assert_eq!(1, proto.optional_int32.unwrap());
        assert_eq!(2, proto.optional_int64.unwrap());
    }

    #[test]
    fn optional_colon() {
        let parse_string = "optional_nested_message: { bb: 1}\n";
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        parse(parse_string.as_bytes(), &mut proto);
        let proto = proto.transcode_to::<TestAllTypes>().unwrap();

        assert_eq!(1, proto.optional_nested_message.unwrap().bb.unwrap());
    }

    #[test]
    fn allow_partial() {
        let mut message = DynamicMessage::new(protobuf_unittest::TestRequired::descriptor());
        let mut parser = Parser::new();
        parser.allow_partial_message(true);
        parser.parse("a: 1".as_bytes(), &mut message);

        assert_eq!(1, message.get_field_by_name("a").unwrap().as_i32().unwrap());
        assert!(!message.has_field_by_name("b"));
        assert!(!message.has_field_by_name("c"));
    }

    #[test]
    fn parse_exotic() {
        let mut message = DynamicMessage::new(TestAllTypes::descriptor());
        let parse_string = "\
            repeated_int32: -1\n\
            repeated_int32: -2147483648\n\
            repeated_int64: -1\n\
            repeated_int64: -9223372036854775808\n\
            repeated_uint32: 4294967295\n\
            repeated_uint32: 2147483648\n\
            repeated_uint64: 18446744073709551615\n\
            repeated_uint64: 9223372036854775808\n\
            repeated_double: 123.0\n\
            repeated_double: 123.5\n\
            repeated_double: 0.125\n\
            repeated_double: 1.23E17\n\
            repeated_double: 1.235E+22\n\
            repeated_double: 1.235e-18\n\
            repeated_double: 123.456789\n\
            repeated_double: inf\n\
            repeated_double: Infinity\n\
            repeated_double: -inf\n\
            repeated_double: -Infinity\n\
            repeated_double: nan\n\
            repeated_double: NaN\n\
            repeated_string: \"\\000\\001\\a\\b\\f\\n\\r\\t\\v\\\\\\'\\\"\"\n\
            repeated_float: 3.4028235e+38\n\
            repeated_float: -3.4028235e+38\n\
            repeated_float: 3.402823567797337e+38\n\
            repeated_float: -3.402823567797337e+38\n";

        assert!(parse_from_str(parse_string, &mut message));

        let message = message.transcode_to::<TestAllTypes>().unwrap();
        assert_eq!(2, message.repeated_int32.len());
        assert_eq!(-1, message.repeated_int32[0]);
        assert_eq!(-2147483648, message.repeated_int32[1]);

        assert_eq!(2, message.repeated_int64.len());
        assert_eq!(-1, message.repeated_int64[0]);
        assert_eq!(-9223372036854775807 - 1, message.repeated_int64[1]);

        assert_eq!(2, message.repeated_uint32.len());
        assert_eq!(4294967295, message.repeated_uint32[0]);
        assert_eq!(2147483648, message.repeated_uint32[1]);

        assert_eq!(2, message.repeated_uint64.len());
        assert_eq!(18446744073709551615, message.repeated_uint64[0]);
        assert_eq!(9223372036854775808, message.repeated_uint64[1]);

        assert_eq!(13, message.repeated_double.len());
        assert_eq!(123.0, message.repeated_double[0]);
        assert_eq!(123.5, message.repeated_double[1]);
        assert_eq!(0.125, message.repeated_double[2]);
        assert_eq!(1.23e17, message.repeated_double[3]);
        assert_eq!(1.235e22, message.repeated_double[4]);
        assert_eq!(1.235e-18, message.repeated_double[5]);
        assert_eq!(123.456789, message.repeated_double[6]);
        assert_eq!(f64::INFINITY, message.repeated_double[7]);
        assert_eq!(f64::INFINITY, message.repeated_double[8]);
        assert_eq!(-f64::INFINITY, message.repeated_double[9]);
        assert_eq!(-f64::INFINITY, message.repeated_double[10]);
        assert!(message.repeated_double[11].is_nan());
        assert!(message.repeated_double[12].is_nan());

        assert_eq!(1, message.repeated_string.len());
        assert_eq!(
            "\x00\x01\x07\x08\x0c\n\r\t\x0b\\\'\"",
            &message.repeated_string[0]
        );

        assert_eq!(4, message.repeated_float.len());
        assert_eq!(f32::MAX, message.repeated_float[0]);
        assert_eq!(-f32::MAX, message.repeated_float[1]);
        assert_eq!(f32::INFINITY, message.repeated_float[2]);
        assert_eq!(-f32::INFINITY, message.repeated_float[3]);
    }

    #[test]
    fn parse_field_value_from_string() {
        let d = TestAllTypes::descriptor();
        let mut message = DynamicMessage::new(d.clone());

        macro_rules! expect_field {
            ($name:ident, $value:expr, $value_str:expr) => {
                paste! {
                    assert!(super::parse_field_value_from_string(
                        $value_str,
                        &d.get_field_by_name(concat!("optional_", stringify!($name))).unwrap(),
                        &mut message)
                    );

                    #[allow(clippy::bool_assert_comparison)]
                    {
                        assert_eq!(
                            $value,
                            message.transcode_to::<TestAllTypes>().unwrap().[<optional_ $name>].unwrap()
                        );
                    }
                }
            };
        }

        macro_rules! expect_float_field {
            ($name:ident, $value:expr, $value_str:expr) => {
                paste! {
                    assert!(super::parse_field_value_from_string(
                        $value_str,
                        &d.get_field_by_name(concat!("optional_", stringify!($name))).unwrap(),
                        &mut message)
                    );
                    approx::assert_ulps_eq!(
                        $value,
                        message.transcode_to::<TestAllTypes>().unwrap().[<optional_ $name>].unwrap(),
                        max_ulps = 4
                    );
                }
            }
        }

        macro_rules! expect_invalid {
            ($name:ident, $value_str:expr) => {
                assert!(!super::parse_field_value_from_string(
                    $value_str,
                    &d.get_field_by_name(concat!("optional_", stringify!($name)))
                        .unwrap(),
                    &mut message
                ));
            };
        }

        expect_field!(int32, 1, "1");
        expect_field!(int32, -1, "-1");
        expect_field!(int32, 0x1234, "0x1234");
        expect_invalid!(int32, "a");
        expect_invalid!(int32, "999999999999999999999999999999999999");
        expect_invalid!(int32, "1.2");

        expect_field!(int64, 1, "1");
        expect_field!(int64, -1, "-1");
        expect_field!(int64, 0x1234567812345678, "0x1234567812345678");
        expect_invalid!(int64, "a");
        expect_invalid!(int64, "999999999999999999999999999999999999");
        expect_invalid!(int64, "1.2");

        expect_field!(uint64, 1, "1");
        expect_field!(uint64, 0x234567812345678, "0x234567812345678");
        expect_invalid!(uint64, "-1");
        expect_invalid!(uint64, "a");
        expect_invalid!(uint64, "999999999999999999999999999999999999");
        expect_invalid!(uint64, "1.2");

        expect_field!(fixed32, 1, "1");
        expect_field!(fixed32, 0x12345678, "0x12345678");
        expect_invalid!(fixed32, "-1");
        expect_invalid!(fixed32, "a");
        expect_invalid!(fixed32, "999999999999999999999999999999999999");
        expect_invalid!(fixed32, "1.2");

        expect_field!(fixed64, 1, "1");
        expect_field!(fixed64, 0x12345678, "0x12345678");
        expect_invalid!(fixed64, "-1");
        expect_invalid!(fixed64, "a");
        expect_invalid!(fixed64, "999999999999999999999999999999999999");
        expect_invalid!(fixed64, "1.2");

        expect_field!(bool, true, "true");
        expect_field!(bool, false, "false");
        expect_field!(bool, true, "1");
        expect_field!(bool, true, "t");
        expect_field!(bool, false, "0");
        expect_field!(bool, false, "f");
        expect_field!(bool, true, "True");
        expect_field!(bool, false, "False");
        expect_invalid!(bool, "tRue");
        expect_invalid!(bool, "faLse");
        expect_invalid!(bool, "2");
        expect_invalid!(bool, "-0");
        expect_invalid!(bool, "on");
        expect_invalid!(bool, "a");

        expect_field!(float, 1f32, "1");
        expect_float_field!(float, 1.5f32, "1.5");
        expect_float_field!(float, 1.5e3f32, "1.5e3");
        expect_float_field!(float, -4.55f32, "-4.55");
        expect_invalid!(float, "a");
        expect_invalid!(float, "1,2");

        expect_field!(double, 1f64, "1");
        expect_field!(double, -1f64, "-1");
        expect_float_field!(double, 2.3, "2.3");
        expect_float_field!(double, 3e5, "3e5");
        expect_invalid!(double, "0xf");
        expect_invalid!(double, "012");

        expect_field!(string, "hello", "\"hello\"");
        expect_field!(string, "-1.87", "'-1.87'");
        expect_invalid!(string, "hello");

        expect_field!(
            nested_enum,
            protobuf_unittest::test_all_types::NestedEnum::Bar as i32,
            "BAR"
        );
        expect_field!(
            nested_enum,
            protobuf_unittest::test_all_types::NestedEnum::Baz as i32,
            &(protobuf_unittest::test_all_types::NestedEnum::Baz as i32).to_string()
        );
        expect_invalid!(nested_enum, "FOOBAR");

        assert!(super::parse_field_value_from_string(
            "<bb:12>",
            &d.get_field_by_name("optional_nested_message").unwrap(),
            &mut message
        ));
        assert_eq!(
            12,
            message
                .transcode_to::<TestAllTypes>()
                .unwrap()
                .optional_nested_message
                .unwrap()
                .bb
                .unwrap()
        );
        expect_invalid!(nested_message, "any");
    }

    #[test]
    fn invalid_token() {
        expect_failure(
            "optional_bool: true\n-5\n",
            "Expected identifier, got: -",
            2,
            1,
        );
        expect_failure(
            "optional_bool: true!\n",
            "Expected identifier, got: !",
            1,
            20,
        );
        expect_failure(
            "\"some string\"",
            "Expected identifier, got: \"some string\"",
            1,
            1,
        );
    }

    #[test]
    fn invalid_field_name() {
        expect_failure(
            "invalid_field: somevalue\n",
            "Message type \"protobuf_unittest.TestAllTypes\" has no field named \"invalid_field\".",
            1,
            14,
        );
    }

    #[test]
    fn invalid_capitalization() {
        expect_failure(
            "optionalgroup {\na: 15\n}\n",
            "Message type \"protobuf_unittest.TestAllTypes\" has no field named \"optionalgroup\".",
            1,
            15,
        );
        expect_failure(
            "OPTIONALgroup {\na: 15\n}\n",
            "Message type \"protobuf_unittest.TestAllTypes\" has no field named \"OPTIONALgroup\".",
            1,
            15,
        );
        expect_failure(
            "Optional_Double: 10.0\n",
            "Message type \"protobuf_unittest.TestAllTypes\" has no field named \"Optional_Double\".",
            1,
            16,
        );
    }

    #[test]
    fn allow_ignore_capitalization_error() {
        let mut parser = Parser::new();
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());

        assert!(!parser.parse_from_str("Optional_Double: 10.0", &mut proto));
        assert!(!parser.parse_from_str("oPtIoNaLgRoUp { a: 15 }", &mut proto));

        parser.allow_case_insensitive_field(true);
        assert!(parser.parse_from_str("Optional_Double: 10.0", &mut proto));
        assert_eq!(
            *proto.get_field_by_name("optional_double").unwrap(),
            Value::F64(10.0)
        );
        assert!(parser.parse_from_str("oPtIoNaLgRoUp { a: 15 }", &mut proto));
        assert_eq!(
            *proto
                .get_field_by_name("optionalgroup")
                .unwrap()
                .as_message()
                .unwrap()
                .get_field_by_name("a")
                .unwrap(),
            Value::I32(15)
        );
    }

    #[test]
    fn invalid_field_values() {
        expect_failure(
            "optional_double: \"hello\"\n",
            "Expected double, got: \"hello\"",
            1,
            18,
        );
        expect_failure(
            "optional_double: true\n",
            "Expected double, got: true",
            1,
            18,
        );
        expect_failure("optional_double: !\n", "Expected double, got: !", 1, 18);
        expect_failure(
            "optional_double: \"hello\"\n",
            "Expected double, got: \"hello\"",
            1,
            18,
        );
        expect_failure(
            "optional_double {\n  \n}\n",
            "Expected \":\", found \"{\".",
            1,
            17,
        );

        expect_failure(
            "optional_int32: \"hello\"\n",
            "Expected integer, got: \"hello\"",
            1,
            17,
        );
        expect_failure(
            "optional_int32: true\n",
            "Expected integer, got: true",
            1,
            17,
        );
        expect_failure("optional_int32: 4.5\n", "Expected integer, got: 4.5", 1, 17);
        expect_failure("optional_int32: !\n", "Expected integer, got: !", 1, 17);
        expect_failure(
            "optional_int32 {\n \n}\n",
            "Expected \":\", found \"{\".",
            1,
            16,
        );
        expect_failure(
            "optional_int32: 0x80000000\n",
            "Integer out of range (0x80000000)",
            1,
            17,
        );
        expect_failure(
            "optional_int64: 0x8000000000000000\n",
            "Integer out of range (0x8000000000000000)",
            1,
            17,
        );
        expect_failure(
            "optional_int32: -0x80000001\n",
            "Integer out of range (0x80000001)",
            1,
            18,
        );
        expect_failure(
            "optional_int64: -0x8000000000000001\n",
            "Integer out of range (0x8000000000000001)",
            1,
            18,
        );

        expect_failure(
            "optional_uint64: \"hello\"\n",
            "Expected integer, got: \"hello\"",
            1,
            18,
        );
        expect_failure(
            "optional_uint64: true\n",
            "Expected integer, got: true",
            1,
            18,
        );
        expect_failure(
            "optional_uint64: 4.5\n",
            "Expected integer, got: 4.5",
            1,
            18,
        );
        expect_failure("optional_uint64: -5\n", "Expected integer, got: -", 1, 18);
        expect_failure("optional_uint64: !\n", "Expected integer, got: !", 1, 18);
        expect_failure(
            "optional_uint64 {\n \n}\n",
            "Expected \":\", found \"{\".",
            1,
            17,
        );
        expect_failure(
            "optional_uint32: 0x100000000\n",
            "Integer out of range (0x100000000)",
            1,
            18,
        );
        expect_failure(
            "optional_uint64: 0x10000000000000000\n",
            "Integer out of range (0x10000000000000000)",
            1,
            18,
        );

        expect_failure(
            "optional_bool: \"hello\"\n",
            "Expected identifier, got: \"hello\"",
            1,
            16,
        );
        expect_failure("optional_bool: 5\n", "Integer out of range (5)", 1, 16);
        expect_failure(
            "optional_bool: -7.5\n",
            "Expected identifier, got: -",
            1,
            16,
        );
        expect_failure("optional_bool: !\n", "Expected identifier, got: !", 1, 16);
        expect_failure(
            "optional_bool: meh\n",
            "Invalid value for boolean field \"optional_bool\". Value: \"meh\".",
            2,
            1,
        );
        expect_failure(
            "optional_bool {\n \n}\n",
            "Expected \":\", found \"{\".",
            1,
            15,
        );

        expect_failure(
            "optional_string: true\n",
            "Expected string, got: true",
            1,
            18,
        );
        expect_failure("optional_string: 5\n", "Expected string, got: 5", 1, 18);
        expect_failure("optional_string: -7.5\n", "Expected string, got: -", 1, 18);
        expect_failure("optional_string: !\n", "Expected string, got: !", 1, 18);
        expect_failure(
            "optional_string {\n \n}\n",
            "Expected \":\", found \"{\".",
            1,
            17,
        );

        expect_failure(
            "optional_nested_enum: \"hello\"\n",
            "Expected integer or identifier, got: \"hello\"",
            1,
            23,
        );
        expect_failure(
            "optional_nested_enum: 5\n",
            "Unknown enumeration value of \"5\" for field \"optional_nested_enum\".",
            2,
            1,
        );
        expect_failure(
            "optional_nested_enum: -7.5\n",
            "Expected integer, got: 7.5",
            1,
            24,
        );
        expect_failure(
            "optional_nested_enum: !\n",
            "Expected integer or identifier, got: !",
            1,
            23,
        );
        expect_failure(
            "optional_nested_enum: grah\n",
            "Unknown enumeration value of \"grah\" for field \"optional_nested_enum\".",
            2,
            1,
        );
        expect_failure(
            "optional_nested_enum {\n \n}\n",
            "Expected \":\", found \"{\".",
            1,
            22,
        );
    }

    #[test]
    fn message_delimiters() {
        expect_failure(
            "OptionalGroup <\n \n}\n",
            "Expected \">\", found \"}\".",
            3,
            1,
        );
        expect_failure(
            "OptionalGroup [\n \n]\n",
            "Expected \"{\", found \"[\".",
            1,
            15,
        );
        expect_failure(
            "optional_nested_message {\n \nbb: 118\n",
            "Expected identifier, got: ",
            4,
            1,
        );
    }

    #[test]
    fn unknown_extension() {
        expect_failure("[blahblah]: 123", "Extension \"blahblah\" is not defined or is not an extension of \"protobuf_unittest.TestAllTypes\".", 1, 11);
    }

    #[test]
    fn missing_required() {
        let mut message = DynamicMessage::new(protobuf_unittest::TestRequired::descriptor());
        expected_failure_from(
            "a: 1",
            "Message missing required fields: b,c",
            1,
            1,
            &mut message,
        );
    }

    #[test]
    fn parse_duplicate_required() {
        let mut message = DynamicMessage::new(protobuf_unittest::TestRequired::descriptor());
        expected_failure_from(
            "a: 1 b: 2 c: 3 a: 1",
            "Non-repeated field \"a\" is specified multiple times.",
            1,
            17,
            &mut message,
        );
    }

    #[test]
    fn parse_duplicate_optional() {
        let mut message = DynamicMessage::new(protobuf_unittest::ForeignMessage::descriptor());
        expected_failure_from(
            "c: 1 c: 2",
            "Non-repeated field \"c\" is specified multiple times.",
            1,
            7,
            &mut message,
        );
    }

    #[test]
    fn merge_duplicate_required() {
        let mut message = DynamicMessage::new(protobuf_unittest::TestRequired::descriptor());
        let parser = Parser::new();
        assert!(parser.merge_from_str("a: 1 b: 2 c: 3 a: 4", &mut message));

        let message = message
            .transcode_to::<protobuf_unittest::TestRequired>()
            .unwrap();
        assert_eq!(4, message.a);
    }

    #[test]
    fn merge_duplicate_optional() {
        let mut message = DynamicMessage::new(protobuf_unittest::ForeignMessage::descriptor());
        let parser = Parser::new();
        assert!(parser.merge_from_str("c: 1 c: 2", &mut message));

        let message = message
            .transcode_to::<protobuf_unittest::ForeignMessage>()
            .unwrap();
        assert_eq!(2, message.c());
    }

    #[test]
    fn explicit_delimiters() {
        let mut message = DynamicMessage::new(protobuf_unittest::TestRequired::descriptor());
        assert!(parse_from_str("a:1,b:2,c:3", &mut message));

        let message = message
            .transcode_to::<protobuf_unittest::TestRequired>()
            .unwrap();
        assert_eq!(1, message.a);
        assert_eq!(2, message.b);
        assert_eq!(3, message.c);
    }

    #[test]
    fn parse_deprecated_field() {
        let mut message =
            DynamicMessage::new(protobuf_unittest::TestDeprecatedFields::descriptor());
        let mut parser = Parser::new();
        expect_message(
            &mut parser,
            "deprecated_int32: 42",
            "WARNING:text format contains deprecated field \"deprecated_int32\"",
            1,
            21,
            &mut message,
            true,
        );
    }

    #[test]
    fn parse_set_recursion_limit() {
        let mut input = String::new();
        for _ in 0..100 {
            input.push_str("child: { ");
        }
        for _ in 0..100 {
            input.push_str(" }");
        }

        let tree = ParseInfoTree::new();
        let mut message = DynamicMessage::new(protobuf_unittest::NestedTestAllTypes::descriptor());
        let mut parser = Parser::new();
        expect_success_and_tree(&mut parser, &input, &mut message, Rc::clone(&tree));

        input = format!("child: {{ {} }}", input);
        parser.set_recursion_limit(100);
        expect_message(
            &mut parser,
            &input,
            "Message is too deep, the parser exceeded the configured recursion limit of 100.",
            1,
            908,
            &mut message,
            false,
        );

        parser.set_recursion_limit(101);
        expect_success_and_tree(&mut parser, &input, &mut message, Rc::clone(&tree));
    }

    #[test]
    fn set_recursion_limit_unknown_field_value() {
        let mut input = String::new();
        for _ in 0..99 {
            input.push('[');
        }
        input.push_str("\"test_value\"");
        for _ in 0..99 {
            input.push(']');
        }

        let mut parser = Parser::new();
        parser.allow_unknown_field(true);
        parser.set_recursion_limit(100);

        let tree = ParseInfoTree::new();
        let mut message = DynamicMessage::new(protobuf_unittest::NestedTestAllTypes::descriptor());
        expect_success_and_tree(
            &mut parser,
            &format!("unknown_nested_array: {}", &input),
            &mut message,
            Rc::clone(&tree),
        );

        let input = format!("unknown_nested_array: [{}]", &input);
        expect_message(
            &mut parser, &input,
            "WARNING:Message type \"protobuf_unittest.NestedTestAllTypes\" has no \
                field named \"unknown_nested_array\".\n\
                1:123: Message is too deep, the parser exceeded the configured recursion limit of 100.",
            1, 21, &mut message, false
        );

        parser.set_recursion_limit(101);
        expect_success_and_tree(&mut parser, &input, &mut message, Rc::clone(&tree));
    }

    #[test]
    fn parse_any_field_with_additional_white_spaces() {
        let mut any = Any::default().transcode_to_dynamic();
        let parse_string = "\
            [type.googleapis.com/protobuf_unittest.TestAllTypes] \t :  \t {\n  \
            optional_int32: 321\n  \
            optional_string: \"teststr0\"\n\
            }\n";
        let finder = Finder::from_descriptors(&[&FILE_DESCRIPTOR]);
        let mut parser = Parser::new();
        parser.set_fineder(finder);

        assert!(parser.parse_from_str(parse_string, &mut any));
    }

    #[test]
    fn parse_extension_field_with_additional_white_spaces() {
        let mut proto = DynamicMessage::new(TestAllExtensions::descriptor());

        let parse_string = "[protobuf_unittest.optional_int32_extension]   : \t 101\n\
            [protobuf_unittest.optional_int64_extension] \t : 102\n";

        let finder = Finder::from_descriptors(&[&FILE_DESCRIPTOR]);
        let mut parser = Parser::new();
        parser.set_fineder(finder);
        assert!(parser.parse_from_str(parse_string, &mut proto));
    }

    #[test]
    fn parse_normal_field_with_additional_white_spaces() {
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());

        let parse_string = "repeated_int32  : \t 1 \n\
            repeated_int32: 2\n\
            repeated_nested_message: {\n  \
              bb: 3\n\
            }\n\
            repeated_nested_message  : \t {\n  \
              bb: 4\n
            }\n\
            repeated_nested_message     {\n  \
              bb: 5\n\
            }\n";
        assert!(parse_from_str(parse_string, &mut proto));
    }

    #[test]
    fn parse_skipped_field_with_additional_white_spaces() {
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        let parse_string = "repeated_int32: 321\n\
            unknown_field1  : \t 12345\n\
            [somewhere.unknown_extension1]   {\n  \
            unknown_field2 \t :  12345\n\
            }\n\
            [somewhere.unknown_extension2]   : \t {\n  \
            unknown_field3   \t :   12345  \n  \
            [somewhere.unknown_extension3]   \t :   {\n  \
            unknwon_field4:  10\n
            }\n\
            [somewhere.unknown_extension4] \t {\n\
            }\n\
            }\n";

        let mut parser = Parser::new();
        parser.allow_unknown_field(true);
        assert!(parser.parse_from_str(parse_string, &mut proto));
    }

    #[test]
    fn test_unknown_field() {
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        let mut parser = Parser::new();
        assert!(!parser.parse_from_str("unknown_field: 12345", &mut proto));
        assert!(!parser.parse_from_str("12345678: 12345", &mut proto));

        parser.allow_unknown_field(true);
        assert!(parser.parse_from_str("unknown_field: 12345", &mut proto));
        assert!(parser.parse_from_str("unknown_field: -12345", &mut proto));
        assert!(parser.parse_from_str("unknown_field: 1.2345", &mut proto));
        assert!(parser.parse_from_str("unknown_field: -1.2345", &mut proto));
        assert!(parser.parse_from_str("unknown_field: 1.2345f", &mut proto));
        assert!(parser.parse_from_str("unknown_field: -1.2345f", &mut proto));
        assert!(parser.parse_from_str("unknown_field: inf", &mut proto));
        assert!(parser.parse_from_str("unknown_field: -inf", &mut proto));
        assert!(parser.parse_from_str("unknown_field: TYPE_STRING", &mut proto));
        assert!(parser.parse_from_str("unknown_field: \"string value\"", &mut proto));
        assert!(!parser.parse_from_str("unknown_field: -TYPE_STRING", &mut proto));
        assert!(parser.parse_from_str(
            "unknown_field: TYPE_STRING\nunknown_field2: 12345",
            &mut proto
        ));
        assert!(parser.parse_from_str(
            "unknown_message1: {}\n\
            unknown_message2 {\n\
              unknown_field: 12345\n\
            }\n\
            unknown_message3 <\n\
              unknown_nested_message {\n\
                unknown_field: 12345\n
              }\n\
            >",
            &mut proto
        ));
        assert!(!parser.parse_from_str("unknown_message: {>", &mut proto));
        assert!(parser.parse_from_str(
            "optional_int32: 1\n\
            unknown_field: 12345\n\
            optional_string: \"string\"\n\
            unknown_message { unknown: 0 }\n\
            optional_nested_message { bb: 2 }",
            &mut proto
        ));
        let p = proto.transcode_to::<TestAllTypes>().unwrap();
        assert_eq!(1, p.optional_int32.unwrap());
        assert_eq!("string", &p.optional_string.unwrap());
        assert_eq!(2, p.optional_nested_message.unwrap().bb.unwrap());

        assert!(parser.parse_from_str("12345678: 12345", &mut proto));

        assert!(parser.parse_from_str(
            "[test.extension1] <\n\
              unknown_nested_message <\n\
                [test.extension2] <\n\
                  unknown_field: 12345\n\
                >\n\
              >\n\
            >",
            &mut proto,
        ));

        assert!(parser.parse_from_str(
            "[test.extension1] {\n\
              unknown_nested_message {\n\
                [test.extension2] {\n\
                  unknown_field: 12345\n\
                }\n\
              }\n\
            }",
            &mut proto,
        ));
        assert!(parser.parse_from_str(
            "[test.extension1] <\n\
              some_unknown_fields: <\n\
                unknown_field: 12345\n\
              >\n\
            >",
            &mut proto,
        ));
        assert!(parser.parse_from_str(
            "[test.extension1] {\n\
              some_unknown_fields: {\n\
                unknown_field: 12345\n\
              }\n\
            }",
            &mut proto,
        ));

        assert!(parser.parse_from_str("unknown_field: [1, 2]", &mut proto));
        assert!(parser.parse_from_str("unknown_field: [VAL1, VAL2]", &mut proto));
        assert!(parser.parse_from_str("unknown_field: [{a:1}, <b:2>]", &mut proto));
    }

    #[test]
    fn test_any_in_unknown_field() {
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        let mut parser = Parser::new();
        parser.allow_unknown_field(true);
        assert!(parser.parse_from_str(
            "unknown {\n\
                [type.googleapis.com/foo.bar] {\n\
                }\n\
            }",
            &mut proto,
        ));
    }

    #[test]
    fn test_unknown_extensions() {
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        let mut parser = Parser::new();
        let message_with_ext = "\
            [test.extension1] {\n\
                some_unknown_field: {\n\
                    unknown_field: 12345\n\
                }\n\
            }";

        assert!(!parser.parse_from_str(message_with_ext, &mut proto));
        parser.allow_unknown_field(true);
        assert!(parser.parse_from_str(message_with_ext, &mut proto));
        parser.allow_unknown_field(false);
        assert!(!parser.parse_from_str(message_with_ext, &mut proto));
        parser.allow_unknown_extension(true);
        assert!(parser.parse_from_str(message_with_ext, &mut proto));
        assert!(!parser.parse_from_str("unknown_field: 1", &mut proto));
    }

    #[test]
    fn test_parse_message_by_field_number() {
        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        let text = "\
            34: 1\n\
            repeated_uint64: 2\n\
        ";
        let mut parser = Parser::new();
        parser.allow_field_number(true);
        assert!(parser.parse_from_str(text, &mut proto));
        let proto = proto.transcode_to::<TestAllTypes>().unwrap();
        assert_eq!(&proto.repeated_uint64, &[1, 2]);

        let mut proto =
            DynamicMessage::new(protobuf_unittest::TestMessageSetContainer::descriptor());
        let text = "\
            1 {\n\
                1545008 {\n\
                    15: 23\n\
                }\n\
                1547769 {\n\
                    25: \"foo\"\n\
                }\n\
            }\n";
        assert!(parser.parse_from_str(text, &mut proto));
        let message = proto.get_field_by_name("message_set").unwrap();
        let message = message.as_message().unwrap();
        let ext1 = message
            .descriptor()
            .get_extension_by_json_name(
                "[protobuf_unittest.TestMessageSetExtension1.message_set_extension]",
            )
            .unwrap();
        let ext2 = message
            .descriptor()
            .get_extension_by_json_name(
                "[protobuf_unittest.TestMessageSetExtension2.message_set_extension]",
            )
            .unwrap();
        assert_eq!(
            23,
            message
                .get_extension(&ext1)
                .as_message()
                .unwrap()
                .get_field_by_name("i")
                .unwrap()
                .as_i32()
                .unwrap()
        );
        assert_eq!(
            "foo",
            message
                .get_extension(&ext2)
                .as_message()
                .unwrap()
                .get_field_by_name("str")
                .unwrap()
                .as_str()
                .unwrap()
        );

        let mut proto = DynamicMessage::new(TestAllTypes::descriptor());
        parser.allow_field_number(false);
        let text = "34:1\n";
        assert!(!parser.parse_from_str(text, &mut proto));

        parser.allow_field_number(true);
        let text = "1234:1\n";
        assert!(!parser.parse_from_str(text, &mut proto));
    }
}
