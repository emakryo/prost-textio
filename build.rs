use std::{io::Read, path::Path};

use prost_build::Config;

fn add_reflection(config: &mut Config, file_descriptor_set_path: impl AsRef<Path>) -> &mut Config {
    let mut buf = vec![];
    std::fs::File::open(file_descriptor_set_path)
        .unwrap()
        .read_to_end(&mut buf)
        .unwrap();

    let descriptor = prost_reflect::FileDescriptor::decode(buf.as_ref()).unwrap();
    let file_descriptor_expr = "crate::FILE_DESCRIPTOR";

    for message in descriptor.all_messages() {
        let full_name = message.full_name();
        config
            .type_attribute(full_name, "#[derive(::prost_reflect::ReflectMessage)]")
            .type_attribute(
                full_name,
                &format!(
                    r#"#[prost_reflect(file_descriptor = "{}", message_name = "{}")]"#,
                    file_descriptor_expr, full_name,
                ),
            );
    }

    config
}

fn main() -> std::io::Result<()> {
    let mut config = prost_build::Config::new();
    let file_descriptor_set_path = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap())
        .join("file_descriptor_set_test.bin");

    config.file_descriptor_set_path(&file_descriptor_set_path);

    config.compile_protos(
        &[
            "tests/proto/google/protobuf/unittest.proto",
            "tests/proto/google/protobuf/unittest_import.proto",
            "tests/proto/google/protobuf/unittest_proto3.proto",
            "tests/proto/google/protobuf/unittest_mset.proto",
            "tests/proto/google/protobuf/unittest_mset_wire_format.proto",
        ],
        &["tests/proto"],
    )?;

    add_reflection(&mut config, &file_descriptor_set_path);

    config.skip_protoc_run().compile_protos(
        &[
            "tests/proto/google/protobuf/unittest.proto",
            "tests/proto/google/protobuf/unittest_import.proto",
            "tests/proto/google/protobuf/unittest_proto3.proto",
            "tests/proto/google/protobuf/unittest_mset.proto",
            "tests/proto/google/protobuf/unittest_mset_wire_format.proto",
        ],
        &["tests/proto"],
    )?;

    Ok(())
}
