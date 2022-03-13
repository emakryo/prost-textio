use once_cell::sync::Lazy;
use prost::Message;
use prost_reflect::FileDescriptor;

pub static FILE_DESCRIPTOR: Lazy<FileDescriptor> = Lazy::new(|| {
    FileDescriptor::new(
        prost_types::FileDescriptorSet::decode(
            include_bytes!(concat!(env!("OUT_DIR"), "/file_descriptor_set_test.bin")).as_ref(),
        )
        .unwrap(),
    )
    .unwrap()
});

#[allow(clippy::all)]
pub mod protobuf_unittest_import {
    include!(concat!(env!("OUT_DIR"), "/protobuf_unittest_import.rs"));
}

#[allow(clippy::all)]
pub mod protobuf_unittest {
    include!(concat!(env!("OUT_DIR"), "/protobuf_unittest.rs"));
}

#[allow(clippy::all)]
pub mod proto2_wireformat_unittest {
    include!(concat!(env!("OUT_DIR"), "/proto2_wireformat_unittest.rs"));
}

#[allow(clippy::all)]
pub mod proto3_unittest {
    include!(concat!(env!("OUT_DIR"), "/proto3_unittest.rs"));
}
