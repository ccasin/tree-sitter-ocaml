/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  prefix: 19,
  dot: 18,
  hash: 17,
  app: 16,
  neg: 15,
  pow: 14,
  mult: 13,
  add: 12,
  cons: 11,
  concat: 10,
  rel: 9,
  and: 8,
  or: 7,
  prod: 6,
  assign: 5,
  if: 4,
  seq: 3,
  match: 2
}

const OP_CHAR = /[!$%&*+\-./:<=>?@^|~]/
const HASH_OP_CHAR = /[#!$%&*+\-./:<=>?@^|~]/
const NUMBER = token(choice(
  /[0-9][0-9_]*(\.[0-9_]*)?([eE][+\-]?[0-9][0-9_]*)?[g-zG-Z]?/,
  /0[xX][0-9A-Fa-f][0-9A-Fa-f_]*(\.[0-9A-Fa-f_]*)?([pP][+\-]?[0-9][0-9_]*)?[g-zG-Z]?/,
  /0[oO][0-7][0-7_]*[g-zG-Z]?/,
  /0[bB][01][01_]*[g-zG-Z]?/
))
// TODO: Make it so that hash numbers must either
//    1) end with a width suffix, as in [#0l],
//    2) contain a dot, as in [#0.] and [#0.0], or
//    3) contain an exponent, as in [#0e0].
// Those are the rules that define the syntax for unboxed numbers in Flambda.
// Examples of what should be valid:
//    #0l
//    #0.
//    #0e0
//    #0.l
//    #0.0
//    #0x0l
//    #0x0.
//    #0o0l
//    #0b0l
// Examples of what should be invalid:
//    #0
//    #0x0
//    #0o0
//    #0b0
// My attempts so far of enforcing these rules have caused problems. I think
// Tree-sitter gets confused in some situations about [toplevel_directive] vs.
// [unboxed_constant].
//
// I think it's okay to leave things as they are for now because Tree-sitter
// successfully parses all unboxed numbers that Flambda can parse -- it just
// parses _more_ than what Flambda can parse.
const HASH_NUMBER = token(seq('#', NUMBER))

module.exports = grammar({
  name: 'ocaml',

  extras: $ => [
    /\s/,
    $.comment,
    $.line_number_directive,
    $.attribute
  ],

  inline: $ => [
    $._parameter,
    $._argument,
    $._extension,
    $._item_extension,
    $._value_name,
    $._label_name,
    $._field_name,
    $._class_name,
    $._class_type_name,
    $._method_name,
    $._type_constructor,
    $._module_name,
    $._module_type_name,
    $._label
  ],

  word: $ => $._identifier,

  supertypes: $ => [
    $._structure_item,
    $._signature_item,
    $._parameter,
    $._module_type,
    $._simple_module_expression,
    $._module_expression,
    $._simple_class_type,
    $._class_type,
    $._class_field_specification,
    $._simple_class_expression,
    $._class_expression,
    $._class_field,
    $._simple_type,
    $._tuple_type,
    $._type,
    $._simple_expression,
    $._expression,
    $._sequence_expression,
    $._simple_pattern,
    $._pattern_no_exception,
    $._pattern,
    $._value_constant,
    $._signed_value_constant,
    $._infix_operator
  ],

  rules: {
    compilation_unit: $ => seq(
      optional($.shebang),
      optional($._structure)
    ),

    shebang: $ => /#!.*/,

    _structure: $ => choice(
      repeat1(';;'),
      seq(
        repeat(';;'),
        choice($._structure_item, $.toplevel_directive, $.expression_item),
        repeat(choice(
          seq(repeat(';;'), choice($._structure_item, $.toplevel_directive)),
          seq(repeat1(';;'), $.expression_item)
        )),
        repeat(';;')
      )
    ),

    expression_item: $ => seq(
      $._sequence_expression,
      repeat($.item_attribute)
    ),

    _signature: $ => choice(
      repeat1(';;'),
      seq(repeat1(seq(repeat(';;'), $._signature_item)), repeat(';;'))
    ),

    // Toplevel

    toplevel_directive: $ => seq(
      $.directive,
      optional(choice(
        $._value_constant, // Cannot be unboxed
        $.value_path,
        $.module_path
      ))
    ),

    // Module implementation

    _structure_item: $ => choice(
      $.value_definition,
      // Value specifications in structures do parse, even though the compiler
      // complains about them
      $.value_specification,
      $.external,
      $.type_definition,
      $.exception_definition,
      $.module_definition,
      $.module_type_definition,
      $.open_module,
      $.include_module,
      $.class_definition,
      $.class_type_definition,
      $.floating_attribute,
      $._item_extension
    ),

    value_definition: $ => seq(
      choice(seq('let', optional($._attribute), optional('rec')), $.let_operator),
      sep1(choice('and', $.let_and_operator), $.let_binding)
    ),

    let_binding: $ => prec.right(seq(
      choice(
        // A pattern. Examples:
        //    let x
        //    let (x @ local)
        //    let (x, y)
        // We cannot allow exception patterns here because that would create a
        // conflict: when encountering [let exception], the parser wouldn't be
        // able to determine whether the code is a [$.let_exception_expression]
        // or an [$.exception_pattern] inside a [$.let_binding].
        field('pattern', $._pattern_no_exception),
        // Legacy modes followed by an identifier or a parenthesized operator.
        // Examples:
        //    let local_ x
        //    let local_ ( + )
        seq(
          $._mode_expr_legacy,
          field('pattern', $._value_name)
        ),
      ),
      optional(choice(
        $.at_mode_expr,
        seq(
          repeat($._parameter),
          optional(choice(
            $._maybe_polymorphic_typed_with_optional_modes,
            $.at_mode_expr
          )),
          optional($._coerced),
          '=',
          field('body', $._sequence_expression)
        )
      )),
      repeat($.item_attribute)
    )),

    _parameter: $ => choice(
      $.parameter,
      alias(
        $._parenthesized_abstract_types_with_optional_jkind_annotations,
        $.abstract_types
      )
    ),

    parameter: $ => choice(
      // A positional parameter. Examples:
      //    x
      //    (x : t)
      //    (x : 'a. 'a)
      field('pattern', $._simple_maybe_polymorphic_pattern_with_optional_modes),
      // A labeled or optional parameter with no additional information.
      // Examples:
      //    ~x
      //    ?x
      seq(
        choice('~', '?'),
        field('pattern', $._simple_value_pattern)
      ),
      // A labeled or optional parameter inside parentheses, optionally with a
      // default value. Examples:
      //    ~(x : t)
      //    ?(x : 'a. 'a)
      //    ?(x : t = y)
      // TODO: Currently, this rule allows default values on labeled
      // parameters, e.g. [~(x = y)], but Flambda doesn't allow this syntax. We
      // should fix this inconsistency.
      seq(
        choice('~', '?'),
        '(',
        optional($._mode_expr_legacy),
        field('pattern', $._simple_value_pattern),
        optional(choice(
          $._maybe_polymorphic_typed_with_optional_modes,
          $.at_mode_expr
        )),
        optional(seq('=', $._sequence_expression)),
        ')'
      ),
      // A labeled or optional parameter with an alias. Examples:
      //    ~x:y
      //    ~x:(y : t)
      //    ?x:(y : 'a. 'a)
      seq(
        $._label,
        token.immediate(':'),
        field(
          'pattern',
          $._simple_maybe_polymorphic_pattern_with_optional_modes
        )
      ),
      // A labeled or optional parameter with an alias and a default value.
      // Examples:
      //    ?x:(y = z)
      //    ?x:(y : t = z)
      //    ?x:(y : 'a. 'a = z)
      // TODO: Currently, this rule allows default values on labeled
      // parameters, e.g. [~x:(y = z)] but Flambda doesn't allow this syntax. We
      // should fix this inconsistency.
      seq(
        $._label,
        token.immediate(':'),
        '(',
        optional($._mode_expr_legacy),
        field('pattern', $._pattern),
        optional(choice(
          $._maybe_polymorphic_typed_with_optional_modes,
          $.at_mode_expr
        )),
        seq('=', $._sequence_expression),
        ')'
      )
    ),

    external: $ => seq(
      'external',
      optional($._attribute),
      $._value_name,
      $._maybe_polymorphic_typed,
      '=',
      repeat1(choice(
        $.string,
        $.quoted_string
      )),
      repeat($.item_attribute)
    ),

    type_definition: $ => seq(
      'type',
      optional($._attribute),
      optional('nonrec'),
      sep1('and', $.type_binding)
    ),

    type_substitution: $ => seq(
      'type',
      optional($._attribute),
      sep1('and', $.type_substitution_binding)
    ),

    type_binding: $ => seq(
      optional($._type_params),
      choice(
        seq(
          field('name', $._type_constructor),
          optional($._jkind_annotation),
          optional(seq(
            '=',
            $._type_binding_body
          )),
          repeat($.type_constraint)
        ),
        seq(
          field('name', $.type_constructor_path),
          seq(
            '+=',
            optional('private'),
            field('body', $.variant_declaration)
          )
        )
      ),
      repeat($.item_attribute)
    ),

    type_substitution_binding: $ => seq(
      optional($._type_params),
      seq(
        field('name', $._type_constructor),
        optional($._jkind_annotation),
        ':=',
        $._type_binding_body,
        repeat($.type_constraint)
      ),
      repeat($.item_attribute)
    ),

    _type_binding_body: $ => choice(
      seq(optional('private'), $._type),
      seq(
        optional($.type_synonym),
        optional('private'),
        field('body', choice(
          $.variant_declaration,
          $.record_declaration,
          '..'
        ))
      )
    ),

    type_synonym: $ => seq(
      $._type,
      '='
    ),

    _type_params: $ => choice(
      $._type_param,
      parenthesize(sep1(',', seq(
        $._type_param,
        optional($._jkind_annotation)
      )))
    ),

    _type_param: $ => seq(
      optional(choice(
        seq('+', optional('!')),
        seq('-', optional('!')),
        seq('!', optional(choice('+', '-')))
      )),
      choice($.type_variable, $._type_wildcard)
    ),

    _type_equation: $ => seq(
      choice(
        seq('=', optional('private')),
        ':='
      ),
      $._type
    ),

    variant_declaration: $ => choice(
      seq('|', sep('|', $.constructor_declaration)),
      sep1('|', $.constructor_declaration)
    ),

    constructor_declaration: $ => seq(
      choice(
        $._constructor_name,
        alias(choice(seq('[', ']'), seq('(', ')'), 'true', 'false'), $.constructor_name)
      ),
      optional(choice(
        seq('of', $._constructor_argument),
        seq(
          ':',
          optional(seq(
            repeat1($._type_variable_with_optional_jkind_annotation),
            '.'
          )),
          optional(seq($._constructor_argument, '->')),
          $._simple_type
        ),
        seq('=', $.constructor_path)
      ))
    ),

    _constructor_argument: $ => choice(
      sep1('*', seq(
        optional($.modality_legacy),
        $._simple_type,
        optional($.atat_modality_expr)
      )),
      $.record_declaration
    ),

    record_declaration: $ => seq(
      '{',
      sep1(';', $.field_declaration),
      optional(';'),
      '}'
    ),

    field_declaration: $ => seq(
      optional(choice(
        'mutable',
        $.modality_legacy
      )),
      $._field_name,
      $._maybe_polymorphic_typed,
      optional($.atat_modality_expr)
    ),

    type_constraint: $ => seq(
      'constraint',
      $._type,
      '=',
      $._type
    ),

    exception_definition: $ => seq(
      'exception',
      optional($._attribute),
      $.constructor_declaration,
      repeat($.item_attribute)
    ),

    module_definition: $ => seq(
      'module', optional($._attribute), optional('rec'),
      sep1('and', $.module_binding)
    ),

    module_binding: $ => seq(
      field('name', choice($._module_name, alias('_', $.module_name))),
      repeat($.module_parameter),
      optional($._module_typed),
      optional(seq(choice('=', ':='), field('body', $._module_expression))),
      repeat($.item_attribute)
    ),

    module_parameter: $ => parenthesize(optional(seq(
      field('name', choice($._module_name, alias('_', $.module_name))),
      $._module_typed
    ))),

    module_type_definition: $ => seq(
      'module', 'type',
      optional($._attribute),
      field('name', $._module_type_name),
      optional(seq(choice('=', ':='), field('body', $._module_type))),
      repeat($.item_attribute)
    ),

    open_module: $ => seq(
      'open',
      optional('!'),
      optional($._attribute),
      $._module_expression,
      repeat($.item_attribute)
    ),

    include_module: $ => seq(
      $._include_maybe_functor,
      optional($._attribute),
      $._module_expression,
      repeat($.item_attribute)
    ),

    _include_maybe_functor: $ => choice(
      'include',
      // Give precedence to "include functor" over "include"
      prec(1, seq('include', 'functor'))
    ),

    class_definition: $ => seq(
      'class', optional($._attribute),
      sep1('and', $.class_binding)
    ),

    class_binding: $ => prec.right(seq(
      optional('virtual'),
      optional(seq(
        '[',
        sep1(',', $._type_param),
        ']'
      )),
      field('name', $._class_name),
      repeat($._parameter),
      optional($._class_typed),
      optional(seq('=', field('body', $._class_expression))),
      repeat($.item_attribute)
    )),

    class_type_definition: $ => seq(
      'class', 'type', optional($._attribute),
      sep1('and', $.class_type_binding)
    ),

    class_type_binding: $ => seq(
      optional('virtual'),
      optional(seq(
        '[',
        sep1(',', $._type_param),
        ']'
      )),
      field('name', $._class_type_name),
      '=',
      field('body', $._simple_class_type),
      repeat($.item_attribute)
    ),

    // Module signature

    _signature_item: $ => choice(
      $.value_specification,
      $.external,
      $.type_definition,
      $.type_substitution,
      $.exception_definition,
      $.module_definition,
      $.module_type_definition,
      $.open_module,
      $.include_module_type,
      $.class_definition,
      $.class_type_definition,
      $.floating_attribute,
      $._item_extension
    ),

    value_specification: $ => seq(
      'val',
      optional($._attribute),
      $._value_name,
      $._maybe_polymorphic_typed_with_optional_modes,
      repeat($.item_attribute)
    ),

    include_module_type: $ => seq(
      $._include_maybe_functor,
      optional($._attribute),
      $._module_type,
      repeat($.item_attribute)
    ),

    // Module types

    _module_typed: $ => seq(':', $._module_type),

    _simple_module_type: $ => choice(
      $.module_type_path,
      $.signature,
      $.parenthesized_module_type
    ),

    _module_type: $ => choice(
      $._simple_module_type,
      $.module_type_constraint,
      $.module_type_of,
      $.functor_type,
      $._extension
    ),

    signature: $ => seq(
      'sig',
      optional($._signature),
      'end'
    ),

    module_type_constraint: $ => prec.right(seq(
      $._module_type,
      'with',
      sep1('and', choice(
        $.constrain_type,
        $.constrain_module,
        $.constrain_module_type,
        $.extended_module_path
      ))
    )),

    constrain_type: $ => seq(
      'type',
      optional($._type_params),
      $.type_constructor_path,
      $._type_equation,
      repeat($.type_constraint)
    ),

    constrain_module: $ => seq(
      'module',
      $.module_path,
      choice('=', ':='),
      $.extended_module_path
    ),

    constrain_module_type: $ => prec.left(seq(
      'module', 'type',
      $.module_type_path,
      choice('=', ':='),
      $._module_type
    )),

    module_type_of: $ => seq(
      'module', 'type', 'of',
      $._module_expression
    ),

    functor_type: $ => prec.right(seq(
      choice(
        seq('functor', repeat($.module_parameter)),
        $._simple_module_type,
        seq('(', ')')
      ),
      '->',
      $._module_type
    )),

    parenthesized_module_type: $ => seq(
      parenthesize($._module_type)
    ),

    // Module expressions

    _simple_module_expression: $ => choice(
      $.typed_module_expression,
      $.parenthesized_module_expression,
      $.packed_module,
      $._extension
    ),

    _module_expression: $ => choice(
      $._simple_module_expression,
      $.module_path,
      $.structure,
      $.functor,
      $.module_application
    ),

    structure: $ => seq(
      'struct',
      optional($._structure),
      'end'
    ),

    functor: $ => prec.right(seq(
      'functor',
      repeat1($.module_parameter),
      '->',
      field('body', $._module_expression),
    )),

    module_application: $ => seq(
      field('functor', $._module_expression),
      choice(
        field('argument', $._simple_module_expression),
        seq('(', ')')
      )
    ),

    typed_module_expression: $ => parenthesize(seq(
      $._module_expression,
      $._module_typed
    )),

    packed_module: $ => parenthesize(seq(
      'val',
      $._expression,
      optional($._module_typed),
      optional(seq(':>', $._module_type))
    )),

    parenthesized_module_expression: $ => parenthesize($._module_expression),

    // Class types

    _class_typed: $ => seq(':', $._class_type),

    _simple_class_type: $ => choice(
      $.class_type_path,
      $.instantiated_class_type,
      $.class_body_type,
      $.let_open_class_type,
      $._extension
    ),

    _class_type: $ => choice(
      $._simple_class_type,
      $.class_function_type
    ),

    instantiated_class_type: $ => seq(
      '[',
      sep1(',', $._type),
      ']',
      $.class_type_path
    ),

    class_body_type: $ => seq(
      'object',
      optional(parenthesize($._type)),
      repeat(choice(
        $._class_field_specification,
        $.floating_attribute
      )),
      'end'
    ),

    _class_field_specification: $ => choice(
      $.inheritance_specification,
      $.instance_variable_specification,
      $.method_specification,
      $.type_parameter_constraint,
      $._item_extension
    ),

    inheritance_specification: $ => seq(
      'inherit',
      $._simple_class_type,
      repeat($.item_attribute)
    ),

    instance_variable_specification: $ => seq(
      'val',
      repeat(choice('mutable', 'virtual')),
      $._instance_variable_name,
      $._typed,
      repeat($.item_attribute)
    ),

    method_specification: $ => seq(
      'method',
      repeat(choice('private', 'virtual')),
      $._method_name,
      $._maybe_polymorphic_typed,
      repeat($.item_attribute)
    ),

    type_parameter_constraint: $ => seq(
      'constraint',
      $._type,
      '=',
      $._type,
      repeat($.item_attribute)
    ),

    let_open_class_type: $ => prec.right(PREC.match, seq(
      'let',
      $.open_module,
      'in',
      field('body', $._simple_class_type)
    )),

    class_function_type: $ => prec.right(PREC.seq, seq(
      $._class_parameter_type,
      '->',
      $._class_type
    )),

    // This is the same as _parameter_type, but polymorphic types and legacy
    // modes are not allowed
    _class_parameter_type: $ => choice(
      alias($.class_typed_label, $.typed_label),
      $._tuple_type_without_leading_label
      // We can't have a tuple_type_with_leading_label here because any leading
      // label should be parsed as the label for the parameter, not for the
      // tuple. Example:
      //    l:t * l:t -> t
      // should be parsed as
      //    l:(t * l:t) -> t
    ),

    // This is the same as typed_label, but legacy modes are not allowed
    class_typed_label: $ => seq(
      optional('?'),
      $._label_name,
      ':',
      $._tuple_type_without_leading_label
    ),

    // Class expressions

    _simple_class_expression: $ => choice(
      $.class_path,
      $.instantiated_class,
      $.object_expression,
      $.typed_class_expression,
      $.parenthesized_class_expression,
      $._extension
    ),

    _class_expression: $ => choice(
      $._simple_class_expression,
      $.class_function,
      $.class_application,
      $.let_class_expression,
      $.let_open_class_expression
    ),

    instantiated_class: $ => seq(
      '[',
      sep1(',', $._type),
      ']',
      $.class_path
    ),

    typed_class_expression: $ => seq(
      parenthesize(seq(
        $._class_expression,
        $._class_typed
      ))
    ),

    class_function: $ => prec.right(PREC.match, seq(
      'fun',
      repeat1($._parameter),
      '->',
      field('body', $._class_expression)
    )),

    class_application: $ => prec.right(PREC.app, seq(
      field('class', $._simple_class_expression),
      repeat1(field('argument', $._argument))
    )),

    let_class_expression: $ => prec.right(PREC.match, seq(
      $.value_definition,
      'in',
      field('body', $._class_expression)
    )),

    _class_field: $ => choice(
      $.inheritance_definition,
      $.instance_variable_definition,
      $.method_definition,
      $.type_parameter_constraint,
      $.class_initializer,
      $._item_extension
    ),

    inheritance_definition: $ => seq(
      'inherit',
      optional('!'),
      $._class_expression,
      optional(seq('as', $._value_pattern)),
      repeat($.item_attribute)
    ),

    instance_variable_definition: $ => seq(
      'val',
      optional('!'),
      repeat(choice('mutable', 'virtual')),
      field('name', $._instance_variable_name),
      optional($._typed),
      optional($._coerced),
      optional(seq('=', field('body', $._sequence_expression))),
      repeat($.item_attribute)
    ),

    method_definition: $ => seq(
      'method',
      optional('!'),
      repeat(choice('private', 'virtual')),
      field('name', $._method_name),
      repeat($._parameter),
      optional($._maybe_polymorphic_typed),
      optional(seq('=', field('body', $._sequence_expression))),
      repeat($.item_attribute)
    ),

    class_initializer: $ => seq(
      'initializer',
      $._sequence_expression,
      repeat($.item_attribute)
    ),

    let_open_class_expression: $ => prec.right(PREC.match, seq(
      'let',
      $.open_module,
      'in',
      field('body', $._class_expression)
    )),

    parenthesized_class_expression: $ => seq(
      parenthesize($._class_expression)
    ),

    // Types

    // Does not allow modes
    _typed: $ => seq(':', $._type),

    _typed_with_optional_modes: $ => seq(
      $._typed,
      optional($.atat_mode_expr)
    ),

    // Does not allow modes
    _simple_typed: $ => seq(':', $._simple_type),

    // Does not allow modes
    _polymorphic_typed: $ => seq(':', $.polymorphic_type),

    _polymorphic_typed_with_optional_modes: $ => seq(
      $._polymorphic_typed,
      optional($.atat_mode_expr)
    ),

    // Does not allow modes
    _maybe_polymorphic_typed: $ => choice(
      $._typed,
      $._polymorphic_typed
    ),

    _maybe_polymorphic_typed_with_optional_modes: $ => choice(
      $._typed_with_optional_modes,
      $._polymorphic_typed_with_optional_modes
    ),

    polymorphic_type: $ => seq(
      choice(
        // Type variables with optional layout annotations. Example:
        //    'a ('b : value)
        repeat1($._type_variable_with_optional_jkind_annotation),
        // Types introduced with the [type] keyword, with optional layout
        // annotations. Example:
        //    type a (b : value)
        alias(
          $._abstract_types_with_optional_jkind_annotations,
          $.abstract_types
        )
      ),
      '.',
      $._type
    ),

    _coerced: $ => seq(':>', $._type),

    // One or more abstract types. Does not allow layout annotations. Example:
    //    type t s
    _abstract_types: $ => seq(
      'type',
      repeat1($._type_constructor)
    ),

    // One or more abstract types. Allows layout annotations but only in
    // parentheses. Example:
    //    type t (s : any)
    _abstract_types_with_optional_jkind_annotations: $ => seq(
      'type',
      repeat1(choice(
        $._type_constructor,
        parenthesize(seq(
          $._type_constructor,
          $._jkind_annotation
        ))
      ))
    ),

    // A single abstract type with a layout annotation. Since there is only one
    // type, we don't need parentheses, i.e. we allow [type t : any] instead of
    // [type (t : any)].
    _abstract_type_with_jkind_annotation: $ => seq(
      'type',
      $._type_constructor,
      $._jkind_annotation
    ),

    // Does not allow layout annotations
    _parenthesized_abstract_types: $ => parenthesize($._abstract_types),

    _parenthesized_abstract_types_with_optional_jkind_annotations: $ =>
      parenthesize(choice(
        $._abstract_types_with_optional_jkind_annotations,
        $._abstract_type_with_jkind_annotation
      )),

    type_variable_with_jkind_annotation: $ => parenthesize(seq(
      $.type_variable,
      $._jkind_annotation
    )),

    type_wildcard_with_jkind_annotation: $ => parenthesize(seq(
      $._type_wildcard,
      $._jkind_annotation
    )),

    _type_variable_with_optional_jkind_annotation: $ => choice(
      $.type_variable,
      $.type_variable_with_jkind_annotation
    ),
    _type_wildcard_with_optional_jkind_annotation: $ => choice(
      $._type_wildcard,
      $.type_wildcard_with_jkind_annotation
    ),

    _jkind_annotation: $ => seq(':', $.jkind),

    // TODO: Add these choices when they are fully implemented in
    // the language.
    //
    // seq(
    //   $.jkind,
    //   'mod',
    //   $.mode_expr
    // ),
    // seq(
    //   $.jkind,
    //   'with',
    //   $._type
    // ),
    // seq(
    //   'kind_of',
    //   $._type
    // )
    jkind: $ => $._identifier,

    _simple_type: $ => choice(
      $.type_variable,
      $.type_constructor_path,
      $.unboxed_type_constructor_path,
      $.constructed_type,
      $.local_open_type,
      $.polymorphic_variant_type,
      $.package_type,
      $.hash_type,
      $.object_type,
      $.parenthesized_type,
      $.type_variable_with_jkind_annotation,
      $.type_wildcard_with_jkind_annotation,
      $._extension
    ),

    _type: $ => choice(
      $._tuple_type,
      $.function_type,
      $.aliased_type
    ),

    function_type: $ => seq(
      $._parameter_type,
      optional($.at_mode_expr),
      '->',
      choice(
        $.function_type,
        seq(
          optional($._mode_expr_legacy),
          $._tuple_type,
          optional($.at_mode_expr)
        )
      )
    ),

    _parameter_type: $ => choice(
      $.typed_label,
      $.polymorphic_typed_label,
      seq(
        optional($._mode_expr_legacy),
        $._tuple_type_without_leading_label
      ),
      // We can't have a tuple_type_with_leading_label here because any leading
      // label should be parsed as the label for the parameter, not for the
      // tuple. Example:
      //    l:t * l:t -> t
      // should be parsed as
      //    l:(t * l:t) -> t
      parenthesize($.polymorphic_type)
    ),

    typed_label: $ => seq(
      optional('?'),
      $._label_name,
      ':',
      optional($._mode_expr_legacy),
      $._tuple_type_without_leading_label
    ),

    polymorphic_typed_label: $ => seq(
      optional('?'),
      $._label_name,
      ':',
      optional($._mode_expr_legacy),
      parenthesize($.polymorphic_type)
    ),

    _tuple_type: $ => prec(PREC.seq, choice(
      $._tuple_type_without_leading_label,
      $.tuple_type_with_leading_label
    )),

    // A tuple type of one or more elements such that the first element is
    // unlabeled
    _tuple_type_without_leading_label: $ => choice(
      $._simple_type,
      $.proper_tuple_type
    ),

    // A tuple type of at least two elements such that the first element is
    // labeled
    tuple_type_with_leading_label: $ => prec(PREC.seq, seq(
      $._label_name,
      ':',
      $.proper_tuple_type
    )),

    // A tuple type of at least two elements such that the first element is
    // unlabeled
    proper_tuple_type: $ => seq(
      $._simple_type,
      '*',
      sep1('*', $._tuple_type_element)
    ),

    _tuple_type_element: $ => prec(PREC.prod, seq(
      optional(seq(
        $._label_name,
        ':'
      )),
      $._simple_type
    )),

    constructed_type: $ => prec(PREC.app, seq(
      $._constructed_type_arguments,
      choice(
        $.type_constructor_path,
        $.unboxed_type_constructor_path
      )
    )),

    _constructed_type_arguments: $ => choice(
      $._simple_type,
      parenthesize(sep2(',', choice(
        // Any type. This includes parenthesized type variables/wildcards with
        // layout annotations. Examples:
        //    t
        //    'a
        //    _
        //    ('a : value)
        //    (_ : value)
        $._type,
        // A type variable with a layout annotation. Example:
        //    'a : value
        seq(
          $.type_variable,
          $._jkind_annotation
        ),
        // A type wildcard with a layout annotation. Example:
        //    _ : value
        seq(
          $._type_wildcard,
          $._jkind_annotation
        )
      )))
    ),

    aliased_type: $ => prec(PREC.match, seq(
      $._type,
      'as',
      choice(
        $._type_variable_with_optional_jkind_annotation,
        $._type_wildcard_with_optional_jkind_annotation
      ),
    )),

    local_open_type: $ => seq(
      $.extended_module_path,
      '.',
      choice(
        parenthesize($._type),
        $.package_type,
        $.polymorphic_variant_type
      )
    ),

    polymorphic_variant_type: $ => seq(
      choice(
        seq('[', $.tag_specification, ']'),
        seq('[', optional($._tag_spec), '|', sep1('|', $._tag_spec), ']'),
        seq('[>', optional('|'), sep('|', $._tag_spec), ']'),
        seq('[<', optional('|'), sep1('|', $._tag_spec), optional(seq('>', repeat1($.tag))), ']'),
      )
    ),

    _tag_spec: $ => choice(
      $._type,
      $.tag_specification
    ),

    tag_specification: $ => seq(
      $.tag,
      optional(seq(
        'of',
        optional('&'),
        sep1('&', $._type)
      ))
    ),

    package_type: $ => parenthesize(seq(
      'module',
      optional($._attribute),
      $._module_type
    )),

    object_type: $ => seq(
      '<',
      optional(choice(
        seq(
          sep1(';', choice(
            $.method_type,
            $._simple_type
          )),
          optional(seq(';', optional('..')))
        ),
        '..'
      )),
      '>'
    ),

    method_type: $ => seq(
      $._method_name,
      $._maybe_polymorphic_typed
    ),

    hash_type: $ => prec(PREC.hash, seq(
      optional($._constructed_type_arguments),
      '#',
      $.class_type_path
    )),

    parenthesized_type: $ => parenthesize($._type),

    // Expressions

    _simple_expression: $ => choice(
      $.value_path,
      $._constant,
      $.typed_expression,
      $.constructor_path,
      $.tag,
      $.list_expression,
      $.array_expression,
      $.iarray_expression,
      $.record_expression,
      $.prefix_expression,
      $.hash_expression,
      $.field_get_expression,
      $.array_get_expression,
      $.string_get_expression,
      $.bigarray_get_expression,
      $.coercion_expression,
      $.local_open_expression,
      $.package_expression,
      $.new_expression,
      $.object_copy_expression,
      $.method_invocation,
      $.object_expression,
      $.parenthesized_expression,
      $.ocamlyacc_value,
      $._extension
    ),

    _expression: $ => choice(
      $._simple_expression,
      $.product_expression,
      $.cons_expression,
      $.application_expression,
      $.infix_expression,
      $.sign_expression,
      $.set_expression,
      $.if_expression,
      $.while_expression,
      $.for_expression,
      $.match_expression,
      $.function_expression,
      $.fun_expression,
      $.try_expression,
      $.let_expression,
      $.assert_expression,
      $.lazy_expression,
      $.let_module_expression,
      $.let_open_expression,
      $.let_exception_expression,
      $.mode_legacy_expression,
      $.exclave_legacy_expression
    ),

    _sequence_expression: $ => choice(
      $._expression,
      $.sequence_expression
    ),

    typed_expression: $ => parenthesize(seq(
      $._sequence_expression,
      $._typed_with_optional_modes
    )),

    // TODO: Consider flattening expressions like a, b, c.

    // A tuple expression
    product_expression: $ => prec.left(PREC.prod, seq(
      field('left', $._product_element),
      ',',
      field('right', $._product_element)
    )),

    _product_element: $ => prec(PREC.prod, choice(
      $._expression,
      seq(
        '~',
        $._label_name,
        optional(seq(
          token.immediate(':'),
          $._simple_expression
        ))
      ),
      seq(
        '~',
        '(',
        $._label_name,
        $._typed,
        ')'
      )
    )),

    cons_expression: $ => prec.right(PREC.cons, seq(
      field('left', $._expression),
      '::',
      field('right', $._expression)
    )),

    list_expression: $ => seq(
      '[',
      optional(choice(
        seq(
          sep1(';', $._expression),
          optional(';')
        ),
        $.comprehension
      )),
      ']'
    ),

    array_expression: $ => seq(
      '[|',
      optional(choice(
        seq(
          sep1(';', $._expression),
          optional(';')
        ),
        $.comprehension
      )),
      '|]'
    ),

    iarray_expression: $ => seq(
      '[:',
      optional(choice(
        seq(
          sep1(';', $._expression),
          optional(';')
        ),
        $.comprehension
      )),
      ':]'
    ),

    comprehension: $ => seq(
      $._expression,
      repeat1($.comprehension_clause)
    ),

    comprehension_clause: $ => choice(
      seq(
        'for',
        sep1('and', $.comprehension_iterator)
      ),
      seq(
        'when',
        $._expression
      )
    ),

    comprehension_iterator: $ => choice(
      seq(
        $._pattern,
        '=',
        $._expression,
        choice('to', 'downto'),
        $._expression
      ),
      seq(
        // We allow legacy modes in [x for local_ x in y], but not in
        // [x for x = 1 to 10]
        optional($.mode_legacy),
        $._pattern,
        'in',
        $._expression
      )
    ),

    record_expression: $ => seq(
      '{',
      optional(seq($._simple_expression, 'with')),
      sep1(';', $.field_expression),
      optional(';'),
      '}'
    ),

    field_expression: $ => prec(PREC.seq, seq(
      field('name', $.field_path),
      optional($._typed),
      optional($._coerced),
      optional(seq('=', field('body', $._expression)))
    )),

    application_expression: $ => prec.right(PREC.app, seq(
      field('function', $._simple_expression),
      repeat1(field('argument', $._argument))
    )),

    _argument: $ => choice(
      $._simple_expression,
      $.labeled_argument
    ),

    labeled_argument: $ => choice(
      $._label,
      seq(
        $._label,
        token.immediate(':'),
        $._simple_expression
      ),
      seq(
        choice('~', '?'),
        '(',
        $._label_name,
        $._typed_with_optional_modes,
        ')'
      ),
    ),

    prefix_expression: $ => prec(PREC.prefix, seq(
      field('operator', $.prefix_operator),
      field('right', $._simple_expression)
    )),

    sign_expression: $ => prec(PREC.neg, seq(
      field('operator', $.sign_operator),
      field('right', $._expression)
    )),

    hash_expression: $ => prec.left(PREC.hash, seq(
      field('left', $._simple_expression),
      field('operator', $.hash_operator),
      field('right', $._simple_expression)
    )),

    infix_expression: $ => {
      const table = [
        {
          operator: $.pow_operator,
          precedence: PREC.pow,
          associativity: 'right'
        },
        {
          operator: $.mult_operator,
          precedence: PREC.mult,
          associativity: 'left'
        },
        {
          operator: $.add_operator,
          precedence: PREC.add,
          associativity: 'left'
        },
        {
          operator: $.concat_operator,
          precedence: PREC.concat,
          associativity: 'right'
        },
        {
          operator: $.rel_operator,
          precedence: PREC.rel,
          associativity: 'left'
        },
        {
          operator: $.and_operator,
          precedence: PREC.and,
          associativity: 'right'
        },
        {
          operator: $.or_operator,
          precedence: PREC.or,
          associativity: 'right'
        },
        {
          operator: $.assign_operator,
          precedence: PREC.assign,
          associativity: 'right'
        }
      ]

      return choice(...table.map(({operator, precedence, associativity}) =>
        prec[associativity](precedence, seq(
          field('left', $._expression),
          field('operator', operator),
          field('right', $._expression)
        ))
      ))
    },

    field_get_expression: $ => prec.left(PREC.dot, seq(
      $._simple_expression,
      '.',
      $.field_path
    )),

    array_get_expression: $ => prec(PREC.dot, seq(
      $._simple_expression,
      '.',
      optional($.indexing_operator_path),
      '(',
      $._sequence_expression,
      ')'
    )),

    // Immutable array indexing is a regular operator, so it is handled by the
    // [indexing_operator_path] rule and doesn't need its own rule

    string_get_expression: $ => prec(PREC.dot, seq(
      $._simple_expression,
      '.',
      optional($.indexing_operator_path),
      '[',
      $._sequence_expression,
      ']'
    )),

    bigarray_get_expression: $ => prec(PREC.dot, seq(
      $._simple_expression,
      '.',
      optional($.indexing_operator_path),
      '{',
      $._sequence_expression,
      '}'
    )),

    set_expression: $ => prec.right(PREC.assign, seq(
      choice(
        $.field_get_expression,
        $.array_get_expression,
        $.string_get_expression,
        $.bigarray_get_expression,
        $._instance_variable_name
      ),
      '<-',
      field('body', $._expression)
    )),

    if_expression: $ => prec.right(PREC.if, seq(
      'if',
      optional($._attribute),
      field('condition', $._sequence_expression),
      $.then_clause,
      optional($.else_clause)
    )),

    then_clause: $ => seq(
      'then',
      $._expression
    ),

    else_clause: $ => seq(
      'else',
      $._expression
    ),

    while_expression: $ => seq(
      'while',
      optional($._attribute),
      field('condition', $._sequence_expression),
      $.do_clause
    ),

    do_clause: $ => seq(
      'do',
      optional($._sequence_expression),
      'done'
    ),

    for_expression: $ => seq(
      'for',
      optional($._attribute),
      field('name', $._pattern),
      '=',
      field('from', $._sequence_expression),
      choice('to', 'downto'),
      field('to', $._sequence_expression),
      $.do_clause
    ),

    sequence_expression: $ => prec.right(PREC.seq, seq(
      field('left', $._expression),
      ';',
      optional(seq(
        optional($._attribute),
        field('right', $._sequence_expression)
      ))
    )),

    match_expression: $ => prec.right(PREC.match, seq(
      choice(
        seq('match', optional($._attribute)),
        $.match_operator
      ),
      $._sequence_expression,
      'with',
      $._match_cases
    )),

    _match_cases: $ => prec.right(seq(
      optional('|'),
      sep1('|', $.match_case)
    )),

    match_case: $ => seq(
      field('pattern', $._pattern),
      optional($.guard),
      '->',
      field('body', choice($._sequence_expression, $.refutation_case))
    ),

    guard: $ => seq(
      'when',
      $._sequence_expression
    ),

    refutation_case: $ => '.',

    function_expression: $ => prec.right(PREC.match, seq(
      'function',
      optional($._attribute),
      $._match_cases
    )),

    fun_expression: $ => prec.right(PREC.match, seq(
      'fun',
      optional($._attribute),
      repeat1($._parameter),
      optional($._simple_typed),
      '->',
      field('body', $._sequence_expression)
    )),

    try_expression: $ => prec.right(PREC.match, seq(
      'try',
      optional($._attribute),
      $._sequence_expression,
      'with',
      $._match_cases
    )),

    let_expression: $ => prec.right(PREC.match, seq(
      $.value_definition,
      'in',
      $._sequence_expression
    )),

    coercion_expression: $ => parenthesize(seq(
      $._sequence_expression,
      optional($._typed),
      $._coerced
    )),

    assert_expression: $ => prec.left(PREC.app, seq(
      'assert',
      optional($._attribute),
      $._simple_expression
    )),

    lazy_expression: $ => prec.left(PREC.app, seq(
      'lazy',
      optional($._attribute),
      $._simple_expression
    )),

    let_module_expression: $ => prec.right(PREC.match, seq(
      'let',
      $.module_definition,
      'in',
      field('body', $._sequence_expression)
    )),

    let_open_expression: $ => prec.right(PREC.match, seq(
      'let',
      $.open_module,
      'in',
      field('body', $._sequence_expression)
    )),

    local_open_expression: $ => seq(
      $.module_path,
      '.',
      choice(
        parenthesize(optional($._sequence_expression)),
        $.list_expression,
        $.array_expression,
        $.iarray_expression,
        $.record_expression,
        $.object_copy_expression,
        $.package_expression
      )
    ),

    package_expression: $ => parenthesize(seq(
      'module',
      optional($._attribute),
      $._module_expression,
      optional($._module_typed)
    )),

    let_exception_expression: $ => prec.right(PREC.match, seq(
      'let',
      $.exception_definition,
      'in',
      field('body', $._sequence_expression)
    )),

    mode_legacy_expression: $ => seq(
      $.mode_legacy,
      $._sequence_expression
    ),

    exclave_legacy_expression: $ => seq(
      $.exclave_legacy,
      $._sequence_expression
    ),

    new_expression: $ => seq(
      'new',
      optional($._attribute),
      $.class_path
    ),

    object_copy_expression: $ => seq(
      '{<',
      sep(';', $.instance_variable_expression),
      optional(';'),
      '>}'
    ),

    instance_variable_expression: $ => seq(
      $._instance_variable_name,
      optional(seq('=', $._expression))
    ),

    method_invocation: $ => prec.right(PREC.hash, seq(
      $._simple_expression,
      '#',
      $._method_name
    )),

    object_expression: $ => seq(
      'object',
      optional($._attribute),
      optional(parenthesize(seq(
        $._pattern,
        optional($._typed)
      ))),
      repeat(choice(
        $._class_field,
        $.floating_attribute
      )),
      'end'
    ),

    parenthesized_expression: $ => choice(
      seq(
        'begin',
        optional($._attribute),
        $._sequence_expression,
        'end'
      ),
      parenthesize($._sequence_expression)
    ),

    ocamlyacc_value: $ => /\$[0-9]+/,

    // Patterns

    _simple_pattern: $ => choice(
      $._value_name,
      $._signed_constant,
      $.typed_pattern,
      $.constructor_path,
      $.tag,
      $.polymorphic_variant_pattern,
      $.record_pattern,
      $.list_pattern,
      $.array_pattern,
      $.iarray_pattern,
      $.local_open_pattern,
      $.package_pattern,
      $.parenthesized_pattern,
      $.pattern_with_modes,
      $.pattern_with_modes_legacy,
      $._extension
    ),

    _pattern_no_exception: $ => choice(
      $._simple_pattern,
      $.alias_pattern,
      $.or_pattern,
      $.constructor_pattern,
      $.tag_pattern,
      $.tuple_pattern,
      $.cons_pattern,
      $.range_pattern,
      $.lazy_pattern
    ),

    _pattern: $ => choice(
      $._pattern_no_exception,
      $.exception_pattern
    ),

    alias_pattern: $ => prec.left(PREC.match, seq(
      $._pattern,
      'as',
      $._value_name
    )),

    // Allows modes
    typed_pattern: $ => parenthesize(seq(
      optional($._mode_expr_legacy),
      $._pattern,
      $._typed_with_optional_modes
    )),

    // Allows modes
    polymorphic_typed_pattern: $ => parenthesize(seq(
      optional($._mode_expr_legacy),
      $._pattern,
      $._polymorphic_typed_with_optional_modes
    )),

    _simple_maybe_polymorphic_pattern_with_optional_modes: $ => choice(
      $._simple_pattern,
      $.polymorphic_typed_pattern
    ),

    pattern_with_modes: $ => parenthesize(seq(
      $._pattern,
      $.at_mode_expr
    )),

    pattern_with_modes_legacy: $ => parenthesize(seq(
      $._mode_expr_legacy,
      $._pattern,
      optional($.at_mode_expr)
    )),

    or_pattern: $ => prec.left(PREC.seq, seq(
      $._pattern,
      '|',
      $._pattern
    )),

    constructor_pattern: $ => prec.right(PREC.app, seq(
      $.constructor_path,
      optional(alias($._parenthesized_abstract_types, $.abstract_types)),
      $._pattern
    )),

    tag_pattern: $ => prec.right(PREC.app, seq(
      $.tag,
      $._pattern
    )),

    polymorphic_variant_pattern: $ => seq(
      '#',
      $.type_constructor_path
    ),

    // TODO: Consider flattening patterns like a, b, c.
    tuple_pattern: $ => prec.left(PREC.prod, seq(
      $._tuple_pattern_element,
      ',',
      choice(
        $._tuple_pattern_element,
        '..'
      )
    )),

    _tuple_pattern_element: $ => prec(PREC.prod, choice(
      $._pattern,
      seq(
        '~',
        $._label_name,
        optional(seq(
          token.immediate(':'),
          $._simple_pattern
        ))
      ),
      seq(
        '~',
        '(',
        $._label_name,
        $._typed,
        ')'
      )
    )),

    record_pattern: $ => prec.left(seq(
      '{',
      sep1(';', $.field_pattern),
      optional(seq(';', '_')),
      optional(';'),
      '}'
    )),

    field_pattern: $ => seq(
      $.field_path,
      optional($._typed),
      optional(seq('=', $._pattern))
    ),

    list_pattern: $ => prec.left(seq(
      '[',
      optional(seq(
        sep1(';', $._pattern),
        optional(';')
      )),
      ']'
    )),

    cons_pattern: $ => prec.right(PREC.cons, seq(
      $._pattern,
      '::',
      $._pattern
    )),

    array_pattern: $ => prec.left(seq(
      '[|',
      optional(seq(
        sep1(';', $._pattern),
        optional(';')
      )),
      '|]'
    )),

    iarray_pattern: $ => prec.left(seq(
      '[:',
      optional(seq(
        sep1(';', $._pattern),
        optional(';')
      )),
      ':]'
    )),

    range_pattern: $ => prec(PREC.dot, seq(
      $._signed_constant,
      '..',
      $._signed_constant
    )),

    lazy_pattern: $ => prec(PREC.hash, seq(
      'lazy',
      optional($._attribute),
      $._pattern
    )),

    local_open_pattern: $ => seq(
      $.module_path,
      '.',
      choice(
        parenthesize(optional($._pattern)),
        $.list_pattern,
        $.array_pattern,
        $.iarray_pattern,
        $.record_pattern
      )
    ),

    package_pattern: $ => parenthesize(seq(
      'module',
      optional($._attribute),
      choice($._module_name, alias('_', $.module_name)),
      optional($._module_typed)
    )),

    parenthesized_pattern: $ => parenthesize($._pattern),

    exception_pattern: $ => seq(
      'exception',
      optional($._attribute),
      $._pattern
    ),

    // Attributes and extensions

    attribute: $ => seq(
      alias(/\[@/, '[@'),
      $.attribute_id,
      optional($.attribute_payload),
      ']'
    ),

    item_attribute: $ => seq(
      '[@@',
      $.attribute_id,
      optional($.attribute_payload),
      ']'
    ),

    floating_attribute: $ => seq(
      '[@@@',
      $.attribute_id,
      optional($.attribute_payload),
      ']'
    ),

    attribute_payload: $ => choice(
      $._structure,
      seq(':', optional(choice($._type, $._signature))),
      seq(
        '?',
        $._pattern,
        optional($.guard)
      )
    ),

    _extension: $ => choice(
      $.extension,
      $.quoted_extension
    ),

    extension: $ => seq(
      '[%',
      $.attribute_id,
      optional($.attribute_payload),
      ']'
    ),

    quoted_extension: $ => seq(
      '{%',
      $.attribute_id,
      optional(/\s+/),
      $._quoted_string,
      '}'
    ),

    _item_extension: $ => choice(
      $.item_extension,
      $.quoted_item_extension
    ),

    item_extension: $ => seq(
      '[%%',
      $.attribute_id,
      optional($.attribute_payload),
      ']',
      repeat($.item_attribute)
    ),

    quoted_item_extension: $ => seq(
      '{%%',
      $.attribute_id,
      optional(/\s+/),
      $._quoted_string,
      '}',
      repeat($.item_attribute)
    ),

    _attribute: $ => seq('%', $.attribute_id),

    // Constants

    _value_constant: $ => choice(
      $.number,
      $.character,
      $.string,
      $.quoted_string,
      $.boolean,
      $.unit
    ),

    unboxed_constant: $ => HASH_NUMBER,

    _constant: $ => choice(
      $._value_constant,
      $.unboxed_constant
    ),

    _signed_value_constant: $ => choice(
      $._value_constant,
      $.signed_number
    ),

    signed_unboxed_constant: $ => seq(
      /[+-]/,
      HASH_NUMBER
    ),

    _signed_constant: $ => choice(
      $._signed_value_constant,
      choice(
        $.unboxed_constant,
        $.signed_unboxed_constant
      )
    ),

    number: $ => NUMBER,

    signed_number: $ => seq(/[+-]/, NUMBER),

    character: $ => seq("'", $.character_content, token.immediate("'")),

    character_content: $ => choice(
      token.immediate(/\r*\n/),
      token.immediate(/[^\\'\r\n]/),
      $._null,
      $.escape_sequence
    ),

    string: $ => seq('"', optional($.string_content), '"'),

    string_content: $ => repeat1(choice(
      token.immediate(/\s/),
      token.immediate(/\[@/),
      /[^\\"%@]+|%|@/,
      $._null,
      $.escape_sequence,
      alias(/\\u\{[0-9A-Fa-f]+\}/, $.escape_sequence),
      alias(/\\\r*\n[\t ]*/, $.escape_sequence),
      $.conversion_specification,
      $.pretty_printing_indication
    )),

    quoted_string: $ => seq('{', $._quoted_string, '}'),

    _quoted_string: $ => seq(
      $._left_quoted_string_delimiter,
      optional($.quoted_string_content),
      $._right_quoted_string_delimiter,
    ),

    quoted_string_content: $ => repeat1(choice(
      token.immediate(/\s/),
      token.immediate(/\[@/),
      /[^%@|]+|%|@|\|/,
      $._null,
      $.conversion_specification,
      $.pretty_printing_indication
    )),

    escape_sequence: $ => choice(
      /\\[\\"'ntbr ]/,
      /\\[0-9][0-9][0-9]/,
      /\\x[0-9A-Fa-f][0-9A-Fa-f]/,
      /\\o[0-3][0-7][0-7]/
    ),

    conversion_specification: $ => token(seq(
      '%',
      optional(/[\-0+ #]/),
      optional(/[1-9][0-9]*|\*/),
      optional(/\.([0-9]*|\*)/),
      choice(
        /[diunlLNxXosScCfFeEgGhHbBat!%@,]/,
        /[lnL][diuxXo]/
      )
    )),

    pretty_printing_indication: $ => /@([\[\], ;.{}?]|\\n|<[0-9]+>)/,

    boolean: $ => choice('true', 'false'),

    unit: $ => choice(
      seq('(', ')'),
      seq('begin', optional($._attribute), 'end')
    ),

    // Operators

    prefix_operator: $ => token(choice(
      seq('!', choice(optional(/[#!$%&*+\-./:<>?@^|~]/), repeat2(HASH_OP_CHAR))),
      seq(/[~?]/, repeat1(HASH_OP_CHAR))
    )),

    sign_operator: $ => choice(/[+-]/, /[+-]\./),

    _infix_operator: $ => choice(
      $.pow_operator,
      $.mult_operator,
      $.add_operator,
      $.concat_operator,
      $.rel_operator,
      $.and_operator,
      $.or_operator,
      $.assign_operator
    ),

    hash_operator: $ => token(seq('#', repeat1(HASH_OP_CHAR))),

    pow_operator: $ => token(choice(
      'lsl', 'lsr', 'asr',
      seq('**', repeat(OP_CHAR))
    )),

    mult_operator: $ => token(choice(
      'mod', 'land', 'lor', 'lxor',
      seq(/[*/%]/, repeat(OP_CHAR))
    )),

    add_operator: $ => choice(
      /[+-]/, /[+-]\./,
      token(choice(
        seq('+', repeat1(OP_CHAR)),
        seq('-', choice(repeat1(/[!$%&*+\-./:<=?@^|~]/), repeat2(OP_CHAR)))
      ))
    ),

    concat_operator: $ => token(
      seq(/[@^]/, repeat(OP_CHAR))
    ),

    rel_operator: $ => token(choice(
      seq(/[=>$]/, repeat(OP_CHAR)),
      seq('<', choice(optional(/[!$%&*+./:<=>?@^|~]/), repeat2(OP_CHAR))),
      seq('&', choice(/[!$%*+\-./:<=>?@^|~]/, repeat2(OP_CHAR))),
      seq('|', choice(/[!$%&*+\-./:<=>?@^~]/, repeat2(OP_CHAR))),
      '!='
    )),

    and_operator: $ => token(choice('&', '&&')),

    or_operator: $ => token(choice('or', '||')),

    assign_operator: $ => /:=/,

    indexing_operator: $ => token(
      seq(/[!$%&*+\-/:=>?@^|]/, repeat(OP_CHAR))
    ),

    indexing_operator_path: $ => path($.module_path, $.indexing_operator),

    let_operator: $ => token(
      seq('let', /[$&*+\-/<=>@^|]/, repeat(OP_CHAR))
    ),

    let_and_operator: $ => token(
      seq('and', /[$&*+\-/<=>@^|]/, repeat(OP_CHAR))
    ),

    match_operator: $ => token(
      seq('match', /[$&*+\-/<=>@^|]/, repeat(OP_CHAR))
    ),

    // Names

    _value_name: $ => choice(
      alias($._identifier, $.value_name),
      $.parenthesized_operator
    ),

    _simple_value_pattern: $ => alias($._identifier, $.value_pattern),

    _value_pattern: $ => choice(
      $._simple_value_pattern,
      $.parenthesized_operator
    ),

    parenthesized_operator: $ => parenthesize(choice(
      $.prefix_operator,
      $._infix_operator,
      $.hash_operator,
      seq(
        '.',
        $.indexing_operator,
        choice(
          seq('(', optional(seq(';', '..')), ')'),
          seq('[', optional(seq(';', '..')), ']'),
          seq('{', optional(seq(';', '..')), '}')
        ),
        optional('<-')
      ),
      $.let_operator,
      $.let_and_operator,
      $.match_operator
    )),

    // Modes

    at_mode_expr: $ => seq(
      '@',
      $._mode_expr
    ),

    atat_mode_expr: $ => seq(
      '@@',
      $._mode_expr
    ),

    atat_modality_expr: $ => seq(
      '@@',
      $.modality_expr
    ),

    _mode_expr: $ => repeat1($._mode),

    modality_expr: $ => repeat1($._modality),

    _mode: $ => alias($._identifier, $.mode),

    _modality: $ => alias($._identifier, $.modality),

    // Legacy modes

    _mode_expr_legacy: $ => repeat1($.mode_legacy),

    mode_legacy: $ => choice(
      'local_',
      'unique_',
      'once_'
    ),

    modality_legacy: $ => 'global_',

    exclave_legacy: $ => 'exclave_',

    // Paths

    value_path: $ => path($.module_path, $._value_name),

    module_path: $ => prec(1, path($.module_path, $._module_name)),

    extended_module_path: $ => choice(
      path($.extended_module_path, $._module_name),
      seq(
        $.extended_module_path,
        parenthesize($.extended_module_path)
      )
    ),

    module_type_path: $ => path($.extended_module_path, $._module_type_name),

    field_path: $ => path($.module_path, $._field_name),

    constructor_path: $ => path($.module_path, $._constructor_name),

    type_constructor_path: $ => choice(
      path($.extended_module_path, $._type_constructor),
      // For some reason, we have to explicitly add the below option, even
      // though it can be matched by the above option. If we don't include this
      // option, Tree-sitter won't be able to parse [type t = (_, t) t].
      //
      // TODO: Continue searching for a better solution.
      '_'
    ),

    unboxed_type_constructor_path: $ => prec(PREC.hash, seq(
      path($.extended_module_path, $._type_constructor),
      token.immediate('#')
    )),

    class_path: $ => path($.module_path, $._class_name),

    class_type_path: $ => path($.extended_module_path, $._class_type_name),

    _label_name: $ => alias($._identifier, $.label_name),
    _field_name: $ => alias($._identifier, $.field_name),
    _class_name: $ => alias($._identifier, $.class_name),
    _class_type_name: $ => alias($._identifier, $.class_type_name),
    _method_name: $ => alias($._identifier, $.method_name),
    _type_constructor: $ => alias($._identifier, $.type_constructor),
    _instance_variable_name: $ => alias($._identifier, $.instance_variable_name),

    _module_name: $ => alias($._capitalized_identifier, $.module_name),
    _module_type_name: $ => alias(choice($._capitalized_identifier, $._identifier), $.module_type_name),
    _constructor_name: $ => choice(
      alias($._capitalized_identifier, $.constructor_name),
      parenthesize(alias('::', $.constructor_name))
    ),

    _identifier: $ => /(\\#)?[a-z_][a-zA-Z0-9_']*/,
    _capitalized_identifier: $ => /[A-Z][a-zA-Z0-9_']*/,

    _label: $ => seq(choice('~', '?'), $._label_name),
    directive: $ => seq(/#/, choice($._identifier, $._capitalized_identifier)),
    type_variable: $ => seq(/'/, choice($._identifier, $._capitalized_identifier)),
    _type_wildcard: $ => alias('_', $.type_variable),
    tag: $ => seq(/`/, choice($._identifier, $._capitalized_identifier)),
    attribute_id: $ => sep1(/\./, choice($._identifier, $._capitalized_identifier))
  },

  externals: $ => [
    $.comment,
    $._left_quoted_string_delimiter,
    $._right_quoted_string_delimiter,
    '"',
    $.line_number_directive,
    $._null
  ]
})

function sep(delimiter, rule) {
  return optional(sep1(delimiter, rule))
}

function sep1(delimiter, rule) {
  return seq(rule, repeat(seq(delimiter, rule)))
}

function sep2(delimiter, rule) {
  return seq(rule, delimiter, sep1(delimiter, rule))
}

function repeat2(rule) {
  return seq(rule, repeat1(rule))
}

function parenthesize(rule) {
  return seq('(', rule, ')')
}

function path(prefix, final) {
  return choice(final, seq(prefix, '.', final))
}
