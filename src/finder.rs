use prost_reflect::{FileDescriptor, MessageDescriptor, ExtensionDescriptor, DynamicMessage, ReflectMessage};


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

    pub fn find_extension(&self, message: &DynamicMessage, name: &str) -> Option<ExtensionDescriptor> {
        let descriptor = message.descriptor();
        let ret = descriptor.extensions().find(|e| e.full_name() == name);

        ret
    }

    pub fn find_any_type(
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

    pub fn find_extension_by_number(
        &self,
        descriptor: &MessageDescriptor,
        number: u32,
    ) -> Option<ExtensionDescriptor> {
        descriptor.get_extension(number)
    }
}
