use crate::ast::*;

#[derive(Debug, PartialEq, Clone)]
pub struct Binding{
    pub id: Id,
    pub val: Value,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Env {
    pub defs: Vec<Binding>,
}

impl Env {
    pub fn empty() -> Env {
        Env { defs: Vec::new() }
    }
    pub fn new(defs : Vec<Binding>) -> Env {
        Env { defs }
    }
    pub fn lookup(&self, id: &Id) -> Option<&Value> {
        for def in self.defs.iter() {
            if def.id == *id {
                return Some(&(def.val));
            }
        }
        None
    }
    pub fn extend(&self, defs: Vec<Binding>) -> Env {
        Env { defs: vec![defs, self.defs.clone()].concat() }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    pub env: Env,
    pub ast: AST,
    pub vars: Vec<Id>,
}

impl Closure {
    pub fn new(env: Env, ast: AST, vars: Vec<Id>) -> Closure {
        Closure { env, ast, vars }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Prim(Prim),
    Int(i64),
    Bool(bool),
    Closure(Closure),
    JamList(JamList),
}

#[derive(Debug, PartialEq, Clone)]
pub enum JamList {
    Empty,
    Cons(Box<Value>, Box<JamList>),
}

pub fn eval(ast: &AST) -> Value {
    eval_aux(&Env::empty(), ast)
}

pub fn eval_aux(env: &Env, ast: &AST) -> Value {
    match ast {
        AST::Id(id) => {
            match env.lookup(id) {
                Some(val) => val.clone(),
                None => panic!("Undefined variable: {:?}", id),
            }
        },
        AST::Prim(prim) => { 
            Value::Prim(*prim)
        },
        AST::ConstantTerm(constant) => {
            match constant {
                Constant::Empty => Value::JamList(JamList::Empty),
                Constant::Int(i) => Value::Int(*i),
                Constant::Bool(b) => Value::Bool(*b),
            }
        },
        AST::If(cond, then, else_) => {
            let cond_val = eval_aux(env, cond);
            match cond_val {
                Value::Bool(true) => eval_aux(env, then),
                Value::Bool(false) => eval_aux(env, else_),
                _ => panic!("if condition not a bool"),
            }
        },
        AST::Let(defs, body) => {
            let evaluated = defs.iter().map(|def| {
                let val = eval_aux(env, &def.ast);
                Binding { id: def.id.clone(), val }
            }).collect();
            let extended_env = env.extend(evaluated);
            eval_aux(&extended_env, body)
        },
        AST::Map(ids, body) => {
            Value::Closure(Closure::new(env.clone(), *body.clone(), ids.clone()))
        },
        AST::App(func, args) => {
            let func_val = eval_aux(env, func);
            let args_vals = args.iter().map(|arg| eval_aux(env, arg)).collect();
            match func_val{
                Value::Prim(prim) => primeval(prim, args_vals),
                Value::Closure(closure) => {
                    let new_env = closure.env.extend(
                        closure.vars.iter().zip(args_vals.iter())
                            .map(|(id, val)| {
                                Binding { id: id.clone(), val: val.clone() }
                                })
                            .collect());
                    eval_aux(&new_env, &closure.ast)
                },
                _ => panic!("applying non-functional value {:?}", func_val),
            }
        },
        AST::UnopApp(unop, arg) => {
            let arg_val = eval_aux(env, arg);
            match unop {
                Unop::Plus => {
                    match arg_val {
                        Value::Int(i) => Value::Int(i),
                        _ => panic!("unop + better be applied to int"),
                    }
                },
                Unop::Minus => {
                    match arg_val {
                        Value::Int(i) => Value::Int(-i),
                        _ => panic!("unop - better be applied to int"),
                    }
                },
                Unop::Not => {
                    match arg_val {
                        Value::Bool(b) => Value::Bool(!b),
                        _ => panic!("unop - better be applied to int"),
                    }
                },
            }
        },
        AST::BinopApp(binop, lhs, rhs) => {
            let lhs_val = eval_aux(env, lhs);
            let rhs_val = eval_aux(env, rhs);
            match binop {
                Binop::Add => {
                    match (lhs_val, rhs_val) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l + r),
                        _ => panic!("binop + better be applied to ints"),
                    }
                },
                Binop::Sub => {
                    match (lhs_val, rhs_val) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l - r),
                        _ => panic!("binop - better be applied to ints"),
                    }
                },
                Binop::Mul => {
                    match (lhs_val, rhs_val) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l * r),
                        _ => panic!("binop * better be applied to ints"),
                    }
                },
                Binop::Div => {
                    match (lhs_val, rhs_val) {
                        (Value::Int(l), Value::Int(r)) => Value::Int(l / r),
                        _ => panic!("binop / better be applied to ints"),
                    }
                },
                Binop::Eq => {
                    Value::Bool(value_equal(lhs_val, rhs_val))
                },
                Binop::Neq => {
                    Value::Bool(!value_equal(lhs_val, rhs_val))
                },
                Binop::Lt => {
                    match (lhs_val, rhs_val) {
                        (Value::Int(l), Value::Int(r)) => Value::Bool(l < r),
                        _ => panic!("binop < better be applied to ints"),
                    }
                },
                Binop::Le => {
                    match (lhs_val, rhs_val) {
                        (Value::Int(l), Value::Int(r)) => Value::Bool(l <= r),
                        _ => panic!("binop <= better be applied to ints"),
                    }
                },
                Binop::Gt => {
                    match (lhs_val, rhs_val) {
                        (Value::Int(l), Value::Int(r)) => Value::Bool(l > r),
                        _ => panic!("binop > better be applied to ints"),
                    }
                },
                Binop::Ge => {
                    match (lhs_val, rhs_val) {
                        (Value::Int(l), Value::Int(r)) => Value::Bool(l >= r),
                        _ => panic!("binop >= better be applied to ints"),
                    }
                },
                Binop::And => {
                    match (lhs_val, rhs_val) {
                        (Value::Bool(l), Value::Bool(r)) => Value::Bool(l && r),
                        _ => panic!("binop && better be applied to bools"),
                    }
                },
                Binop::Or => {
                    match (lhs_val, rhs_val) {
                        (Value::Bool(l), Value::Bool(r)) => Value::Bool(l || r),
                        _ => panic!("binop || better be applied to bools"),
                    }
                },
            }
        },
    }
}

fn value_equal(lhs: Value, rhs: Value) -> bool {
    match (lhs, rhs) {
        (Value::Int(l), Value::Int(r)) => l == r,
        (Value::Bool(l), Value::Bool(r)) => l == r,
        (Value::JamList(l), Value::JamList(r)) => 
            match (l, r) {
                (JamList::Empty, JamList::Empty) => true,
                (JamList::Cons(lhs, ltail), JamList::Cons(rhs, rtail)) =>
                    value_equal(*lhs, *rhs) && value_equal(Value::JamList(*ltail), Value::JamList(*rtail)),
                _ => false,
            },
        _ => false,
    }
}

fn primeval(f: Prim, args: Vec<Value>) -> Value {
    // first check if the number of arguments is correct
    let num_args = args.len();
    if num_args != prim_arity(&f){
        panic!("wrong number of arguments to prim {:?}. arity: {:?} but were given {:?}", f, prim_arity(&f), num_args);
    }
    // then apply the function
    match f {
        Prim::NumberQ => {
            match args[0] {
                Value::Int(_) => Value::Bool(true),
                _ => Value::Bool(false),
            }
        },
        Prim::FunctionQ => {
            match args[0] {
                Value::Prim(_) | Value::Closure(_) => Value::Bool(true),
                _ => Value::Bool(false),
            }
        },
        Prim::ListQ => {
            match args[0] {
                Value::JamList(_) => Value::Bool(true),
                _ => Value::Bool(false),
            }
        },
        Prim::EmptyQ => {
            match &args[0] {
                Value::JamList(JamList::Empty) => Value::Bool(true),
                _ => Value::Bool(false),
            }
        },
        Prim::ConsQ => {
            match &args[0] {
                Value::JamList(JamList::Cons(_, _)) => Value::Bool(true),
                _ => Value::Bool(false),
            }
        },
        Prim::First => {
            match &args[0] {
                Value::JamList(JamList::Cons(v, _)) => *v.clone(),
                _ => panic!("first applied to non-list"),
            }
        },
        Prim::Rest => {
            match &args[0] {
                Value::JamList(JamList::Cons(_, v)) => Value::JamList(*v.clone()),
                _ => panic!("rest applied to non-list"),
            }
        },
        Prim::Cons => {
            let head = args[0].clone();
            let tail = args[1].clone();
            match tail {
                Value::JamList(tail_) => Value::JamList(JamList::Cons(Box::new(head), Box::new(tail_))),
                _ => panic!("cons applied to non-list"),
            }
        },
        Prim::Arity => {
            match &args[0] {
                Value::Prim(p) => Value::Int(prim_arity(p) as i64),
                Value::Closure(c) => Value::Int(c.vars.len() as i64),
                _ => panic!("arity applied to non-function"),
                }
        }
    }
}

fn prim_arity(p: &Prim) -> usize {
    match p {
        Prim::NumberQ | Prim::FunctionQ
        | Prim::ListQ | Prim::EmptyQ | Prim::ConsQ 
        | Prim::First | Prim::Rest | Prim::Arity => 1,
        Prim::Cons => 2,
    }
}