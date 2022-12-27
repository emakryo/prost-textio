use std::{io::{Write, Cursor}, collections::HashMap, hash::{Hash, Hasher}, any::Any, marker::PhantomData, rc::Rc};

use prost_reflect::{DynamicMessage, FieldDescriptor, MessageDescriptor, ReflectMessage, OneofDescriptor};
use prost_types::{FileDescriptorSet, FieldDescriptorProto, DescriptorProto};
use crate::{Error, Result, finder::Finder};

const DEBUG_STRING_SILENT_MARKER: &str = "\t ";

trait OneofDescriptorExt {
    fn is_synthetic(&self) -> bool;
}

impl OneofDescriptorExt for OneofDescriptor {
    fn is_synthetic(&self) -> bool {
        if self.fields().count() == 1 {
            let field = self.fields().nth(0).unwrap();
            field.field_descriptor_proto().proto3_optional()
        } else {
            false
        }
    }
}



fn in_real_oneof(field: &FieldDescriptor) -> bool {
    field
        .containing_oneof()
        .map_or(false, |o| !o.is_synthetic())
}

trait BaseTextGenerator {
    fn indent(&mut self);
    fn outdent(&mut self);
    fn get_current_indenteation_size(&self) -> usize {
        0
    }

    fn print(&mut self, text: &[u8], size: usize);
    fn print_stinrg(&mut self, str: &str) {
        self.print(str.as_bytes(), str.len());
    }

    fn print_literal(&mut self, text: &str) {
        self.print_stinrg(text);
    }
}


struct TextGenerator<W> {
    output: W,
    buffer: (),
    buffer_size: usize,
    at_start_of_line: bool,
    failed: bool,
    insert_silent_marker: bool,
    indent_level: usize,
    initial_indent_level: usize,
}

impl<W:Write> TextGenerator<W> {
    fn new(output: W, insert_silent_marker: bool, initial_indent_level: usize) -> Self {
        Self {
            output,
            buffer: (),
            buffer_size: 0,
            at_start_of_line: true,
            failed: false,
            insert_silent_marker,
            indent_level: initial_indent_level,
            initial_indent_level,
        }
    }

    fn print(&self, text: &str) {
        todo!()
    }

    fn print_literal(&self, text: &str) {
        todo!()
    }

    fn print_maybe_with_marker(&self, text_head: &str, text_tail: &str) {
        self.print(text_head);
        if self.consume_insert_silent_marker() {
            self.print_literal(DEBUG_STRING_SILENT_MARKER);
        }
    }

    fn failed(&self) -> Result<()> {
        todo!()
    }

    fn consume_insert_silent_marker(&self) -> bool {
        todo!()
    }
}

impl<W: Write> BaseTextGenerator for TextGenerator<W> {
    fn indent(&mut self) {
        todo!()
    }

    fn outdent(&mut self) {
        todo!()
    }

    fn get_current_indenteation_size(&self) -> usize {
        todo!()
    }

    fn print(&mut self, text: &[u8], size: usize) {
        todo!()
    }
}

pub struct Printer {
    initial_indent_level: usize,
    single_line_mode: bool,
    use_field_number: bool,
    use_short_repeated_primitives: bool,
    hide_unknown_fields: bool,
    print_message_fields_in_index_order: bool,
    expand_any: bool,
    finder: Finder,
    truncate_string_field_longer_than: u64,
    insert_silent_marker: bool,
    custorm_printers: HashMap<*const FieldDescriptorProto, Box<dyn FieldValuePrinter>>,
    default_field_value_printer: Box<dyn FieldValuePrinter>,
    custom_message_printers: HashMap<*const DescriptorProto, Box<dyn MessagePrinter>>,
}


pub struct UnknownFieldSets {

}

pub trait FieldValuePrinter {
}

impl FieldValuePrinter for () {
}

trait FastFieldValuePrinter {
    fn print_bool(val: bool, generator: &mut dyn BaseTextGenerator);
}


pub struct FieldValuePrinterUtf8Escaping;

impl FieldValuePrinterUtf8Escaping {
    fn new() -> Self {
        Self
    }

    fn print_string(&self, val: &str, generator: &mut dyn BaseTextGenerator) {

    }
}

impl FieldValuePrinter for FieldValuePrinterUtf8Escaping {
}

pub struct DebugStringFieldValuePrinter;

impl FieldValuePrinter for DebugStringFieldValuePrinter {
}

impl DebugStringFieldValuePrinter {
    fn new() -> Self {
        Self
    }

    fn print_message_start(_: &DynamicMessage, _: usize, _: usize, single_line_mode: bool, generator: &mut dyn BaseTextGenerator) {
        // let generator = generator.downcast_mut::<TextGenerator<W>>().unwrap();
        // if single_line_mode {
        //     generator.print_maybe_with_marker(" ", "{ ");
        // } else {
        //     generator.print_maybe_with_marker(" ", "{\n");
        // }
        todo!()
    }
}


trait MessagePrinter {
    fn print(&self, message: &DynamicMessage, single_line_mode: bool, generator: &mut dyn BaseTextGenerator) {
        todo!()
    }
}

impl Default for Printer
{
    fn default() -> Self {
        let mut this = Self {
            initial_indent_level: 0,
            single_line_mode: false,
            use_field_number: false,
            use_short_repeated_primitives: false,
            insert_silent_marker: false,
            hide_unknown_fields: false,
            print_message_fields_in_index_order: false,
            expand_any: false,
            truncate_string_field_longer_than: 0,
            finder: Finder::default(),
            custorm_printers: HashMap::new(),
            default_field_value_printer: Box::new(()),
            custom_message_printers: HashMap::new(),
        };

        this.set_use_utf8_string_escape(false);
        
        this
    }
}

impl Printer
{
    pub fn new() -> Self {
        todo!()
    }

    pub fn print<W: Write>(&self, message: &DynamicMessage, output: W) -> Result<()>
    {
        let mut generator = TextGenerator::new(
            output,
            self.insert_silent_marker,
            self.initial_indent_level,
        );

        self.print_inner(message, &mut generator);

        generator.failed()?;

        Ok(())
    }

    pub fn print_to_string(&self, message: &DynamicMessage) -> Result<String> {
        let mut buf = Vec::new();
        self.print(message, &mut buf)?;

        Ok(String::from_utf8_lossy(&buf).into_owned())
    }

    pub fn print_unknown_fields(&self, unknown_fields: &UnknownFieldSets, output: Box<dyn Write>) -> Result<()> {
        todo!()
    }

    pub fn print_unknown_fields_to_string(&self, unknonw_fields: &UnknownFieldSets, output: &mut String) -> Result<()> {
        todo!()
    }

    pub fn print_field_value_to_string(&self, message: &DynamicMessage, field: &FieldDescriptor, index: usize, output: &mut String) -> Result<()> {
        todo!()
    }

    pub fn set_initial_indent_level(&mut self, indent_level: usize) {
        self.initial_indent_level = indent_level;
    }

    pub fn set_single_line_mode(&mut self, single_line_mode: bool) {
        self.single_line_mode = single_line_mode;
    }

    pub fn is_in_single_line_mode(&self) -> bool {
        self.single_line_mode
    }

    pub fn set_use_field_number(&mut self, use_field_number: bool) {
        self.use_field_number = use_field_number;
    }

    pub fn set_use_short_repeated_primitives(&mut self, use_short_repeated_primitives: bool) {
        self.use_short_repeated_primitives = use_short_repeated_primitives;
    }

    pub fn set_use_utf8_string_escape(&mut self, as_utf8: bool) {
        self.set_default_field_value_printer(if as_utf8 {
            Box::new(FieldValuePrinterUtf8Escaping::new()) as Box<dyn FieldValuePrinter>
        } else {
            Box::new(DebugStringFieldValuePrinter::new()) as Box<dyn FieldValuePrinter>
        });
    }

    pub fn set_default_field_value_printer(&mut self, printer: Box<dyn FieldValuePrinter>) {
        self.default_field_value_printer = printer;
    }

    pub fn set_hide_unknown_fields(&mut self, hide: bool) {
        self.hide_unknown_fields = hide;
    }

    pub fn set_print_message_fields_in_index_order(&mut self, print_message_fields_in_index_order: bool) {
        self.print_message_fields_in_index_order = print_message_fields_in_index_order;
    }

    pub fn set_expand_any(&mut self, expand: bool) {
        self.expand_any = expand;
    }

    pub fn set_finder(&mut self, finder: Finder) {
        self.finder = finder;
    }

    pub fn set_truncate_string_field_longer_than(&mut self, truncate_string_field_longer_than: u64) {
        self.truncate_string_field_longer_than = truncate_string_field_longer_than;
    }

    pub fn regeister_field_value_printer(&mut self, field: FieldDescriptor, printer: Box<dyn FieldValuePrinter>) -> bool {
        todo!()
    }

    pub fn register_message_printer(&mut self, desriptor: MessageDescriptor, printer: Box<dyn MessagePrinter>) -> bool {
        todo!()
    }

    fn print_inner<W: Write>(&self, message: &DynamicMessage, generator: &mut TextGenerator<W>) {
        let descriptor = message.descriptor();
        if let Some(printer) = self.custom_message_printers.get(&(descriptor.descriptor_proto() as *const _)) {
            printer.print(message, self.single_line_mode, generator);
            return;
        }

        if descriptor.full_name() == prost_types::Any::default().descriptor().full_name() && self.expand_any && self.print_any(message, generator) {
            return;
        }

        let mut fields = vec![];
        if descriptor.is_map_entry() {
            fields.push(descriptor.get_field(0).unwrap());
            fields.push(descriptor.get_field(1).unwrap());
        } else {
            for field in descriptor.fields() {
                
                if field.is_list() {
                    if message.has_field(&field) {
                        fields.push(field)
                    }
                } else {
                    let containing_oneof = field.containing_oneof();
                    if in_real_oneof(&field) {
                        todo!()
                    }
                }
            }
        }

    }

    fn print_field(&self, message: &DynamicMessage, field: &FieldDescriptor, generator: &mut dyn BaseTextGenerator) {
        todo!()
    }

    fn print_short_releated_field(&self, message: &DynamicMessage, field: &FieldDescriptor, generator: &mut dyn BaseTextGenerator) {
        todo!()
    }

    fn print_field_name(&self, message: &DynamicMessage, field_index: usize, field_count: usize, field: &FieldDescriptor, generator: &mut dyn BaseTextGenerator) {
        todo!()
    }

    fn print_field_value(&self, message: &DynamicMessage, field: &FieldDescriptor, index: usize, generator: &mut dyn BaseTextGenerator) {
        todo!()
    }

    fn print_unknown_fields_inner(&self, unknown_fields: &UnknownFieldSets, generator: &mut dyn BaseTextGenerator, recursion_budget: usize) {
        todo!()
    }

    fn print_any(&self, message: &DynamicMessage, generator: &mut dyn BaseTextGenerator) -> bool {
        todo!()
    }

    fn get_field_printer(&self, field: &FieldDescriptor) -> &dyn FieldValuePrinter {
        self.custorm_printers.get(&(field.field_descriptor_proto() as *const _))
            .unwrap_or(&self.default_field_value_printer)
            .as_ref()
    }
}

pub fn print<W: Write>(message: &DynamicMessage, output: W) -> Result<()> {
    Printer::default().print(message, output)
}

pub fn print_to_string(message: &DynamicMessage) -> Result<String> {
    Printer::default().print_to_string(message)
}


#[cfg(test)]
mod tests {
    use prost_reflect::ReflectMessage;

    use super::*;
    use crate::test_util::{*, protobuf_unittest::TestAllTypes};

    #[test]
    fn test_basic() {
        let proto = TestAllTypes::default().transcode_to_dynamic();

        let expected = std::fs::read_to_string("tests/text_format_unittest_data_oneof_implemented.txt").unwrap();
        let actual = print_to_string(&proto).unwrap();
    }
}
