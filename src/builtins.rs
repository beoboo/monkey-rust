use crate::object::{Array, Error, Integer, NULL, Object, ObjectType, StringE};

pub fn len_builtin(args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    if args.len() != 1 {
        return Some(Box::new(Error::new(format!("wrong number of arguments. got={}, want=1", args.len()))));
    }

    match args[0].get_type() {
        ObjectType::Array => {
            let arg = args[0].as_any().downcast_ref::<Array>().unwrap();
            Some(Box::new(Integer { value: arg.elements.len() as i64 }))
        }
        ObjectType::String => {
            let arg = args[0].as_any().downcast_ref::<StringE>().unwrap();
            Some(Box::new(Integer { value: arg.value.len() as i64 }))
        }
        _ => Some(Box::new(Error::new(format!("argument to \"len\" not supported, got {:?}", args[0].get_type()))))
    }
}

pub fn puts_builtin(args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    for arg in args {
        println!("{}", arg.inspect());
    }

    Some(Box::new(NULL))
}

pub fn first_builtin(args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    if args.len() != 1 {
        return Some(Box::new(Error::new(format!("wrong number of arguments. got={}, want=1", args.len()))));
    }

    if args[0].get_type() != ObjectType::Array {
        return Some(Box::new(Error::new(format!("argument to \"first\" must be Array, got {:?}", args[0].get_type()))));
    }

    let array = args[0].as_any().downcast_ref::<Array>().unwrap();
    if array.elements.len() > 0 {
        Some(array.elements[0].clone())
    } else {
        Some(Box::new(NULL))
    }
}

pub fn last_builtin(args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    if args.len() != 1 {
        return Some(Box::new(Error::new(format!("wrong number of arguments. got={}, want=1", args.len()))));
    }

    if args[0].get_type() != ObjectType::Array {
        return Some(Box::new(Error::new(format!("argument to \"last\" must be Array, got {:?}", args[0].get_type()))));
    }

    let array = args[0].as_any().downcast_ref::<Array>().unwrap();
    let length = array.elements.len();
    if length > 0 {
        Some(array.elements[length - 1].clone())
    } else {
        Some(Box::new(NULL))
    }
}

pub fn rest_builtin(args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    if args.len() != 1 {
        return Some(Box::new(Error::new(format!("wrong number of arguments. got={}, want=1", args.len()))));
    }

    if args[0].get_type() != ObjectType::Array {
        return Some(Box::new(Error::new(format!("argument to \"rest\" must be Array, got {:?}", args[0].get_type()))));
    }

    let array = args[0].as_any().downcast_ref::<Array>().unwrap();
    let length = array.elements.len();
    if length > 0 {
        let elements = array.elements[1..length].to_vec();
        Some(Box::new(Array { elements }))
    } else {
        Some(Box::new(NULL))
    }
}

pub fn push_builtin(args: Vec<Box<dyn Object>>) -> Option<Box<dyn Object>> {
    if args.len() != 2 {
        return Some(Box::new(Error::new(format!("wrong number of arguments. got={}, want=2", args.len()))));
    }

    if args[0].get_type() != ObjectType::Array {
        return Some(Box::new(Error::new(format!("argument to \"push\" must be Array, got {:?}", args[0].get_type()))));
    }

    let array = args[0].as_any().downcast_ref::<Array>().unwrap();
    let item = args[1].clone();
    let mut elements = array.elements.clone();
    elements.push(item);

    Some(Box::new(Array { elements }))
}
