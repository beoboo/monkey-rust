use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SymbolScope {
    GLOBAL,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub(crate) index: usize,
}

pub struct SymbolTable {
    symbols: HashMap<String, Symbol>,
    counter: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            counter: 0,
        }
    }

    pub fn define<S: Into<String>>(&mut self, name: S) -> Symbol {
        let name = name.into();
        let symbol = Symbol{name: name.clone(), scope: SymbolScope::GLOBAL, index: self.counter};
        self.symbols.insert(name, symbol.clone());

        self.counter += 1;

        symbol
    }

    pub fn resolve(&self, name: &String) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define() {
        let expected : HashMap<String, Symbol> = vec![
            ("a".to_string(), Symbol{name: "a".to_string(), scope: SymbolScope::GLOBAL, index: 0}),
            ("b".to_string(), Symbol{name: "b".to_string(), scope: SymbolScope::GLOBAL, index: 1}),
        ].into_iter().collect();

        let mut global = SymbolTable::new();
        let a = global.define("a");
        assert_eq!(a, expected["a"]);

        let b = global.define("b");
        assert_eq!(b, expected["b"]);
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let expected : Vec<Symbol> = vec![
            Symbol{name: "a".to_string(), scope: SymbolScope::GLOBAL, index: 0},
            Symbol{name: "b".to_string(), scope: SymbolScope::GLOBAL, index: 1},
        ];

        for symbol in expected {
            match global.resolve(&symbol.name) {
                Some(s) => assert_eq!(*s, symbol),
                None => panic!("Name {} not resolvable", symbol.name),
            }
        }
    }
}