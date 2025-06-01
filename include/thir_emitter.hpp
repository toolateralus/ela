#pragma once
#include "builder.hpp"
#include "thir.hpp"
#include "error.hpp"

struct THIREmitter {
	StringBuilder code{};
	int indent_level = 0;

	void emit_program(THIR *thir) {
		throw_error("emit_progam is unimplemented", thir->source_range);
	}
	void emit_bin_expr(THIR *thir) {
		throw_error("emit_bin_expr is unimplemented", thir->source_range);
	}
	void emit_unary_expr(THIR *thir) {
		throw_error("emit_unary_expr is unimplemented", thir->source_range);
	}
	void emit_literal(THIR *thir) {
		throw_error("emit_literal is unimplemented", thir->source_range);
	}
	void emit_call(THIR *thir) {
		throw_error("emit_call is unimplemented", thir->source_range);
	}
	void emit_member_access(THIR *thir) {
		throw_error("emit_member_access is unimplemented", thir->source_range);
	}
	void emit_cast(THIR *thir) {
		throw_error("emit_cast is unimplemented", thir->source_range);
	}
	void emit_index(THIR *thir) {
		throw_error("emit_index is unimplemented", thir->source_range);
	}
	void emit_aggregate_initializer(THIR *thir) {
		throw_error("emit_aggregate_initializer is unimplemented", thir->source_range);
	}
	void emit_collection_initializer(THIR *thir) {
		throw_error("emit_collection_initializer is unimplemented", thir->source_range);
	}
	void emit_empty_initializer(THIR *thir) {
		throw_error("emit_empty_initializer is unimplemented", thir->source_range);
	}
	void emit_size_of(THIR *thir) {
		throw_error("emit_size_of is unimplemented", thir->source_range);
	}
	void emit_return_stmt(THIR *thir) {
		throw_error("emit_return_stmt is unimplemented", thir->source_range);
	}
	void emit_break_stmt(THIR *thir) {
		throw_error("emit_break_stmt is unimplemented", thir->source_range);
	}
	void emit_continue_stmt(THIR *thir) {
		throw_error("emit_continue_stmt is unimplemented", thir->source_range);
	}
	void emit_for_stmt(THIR *thir) {
		throw_error("emit_for_stmt is unimplemented", thir->source_range);
	}
	void emit_if_stmt(THIR *thir) {
		throw_error("emit_if_stmt is unimplemented", thir->source_range);
	}
	void emit_while_stmt(THIR *thir) {
		throw_error("emit_while_stmt is unimplemented", thir->source_range);
	}
	void emit_switch_stmt(THIR *thir) {
		throw_error("emit_switch_stmt is unimplemented", thir->source_range);
	}
	void emit_variable(THIR *thir) {
		throw_error("emit_variable is unimplemented", thir->source_range);
	}
	void emit_block(THIR *thir) {
		throw_error("emit_block is unimplemented", thir->source_range);
	}
};