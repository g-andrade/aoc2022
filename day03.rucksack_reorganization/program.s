@ vim:ft=armv5
		.global _start
		.func _start

/* Raspberry PI ARM assembly */

_start:
	PUSH {IP, LR}
_next_line:
	// Get line using fgets
	PUSH {R0-R2}
	LDR R0, =last_input_line
	MOV R1, #(256 - 1) // size of 'last_input_line' buffer - 1 (how to define constants?)
	LDR R2, =stdin
	LDR R2, [R2]
	BL fgets
	CMP R0, #0
	POP {R0-R2}
	BEQ _finished // failed to read line
	BL _log_rucksack_scanned

	// Determine halfway point (where compartments split) using strlen
	PUSH {R0-R1}
	LDR R0, =last_input_line
	BL strlen @ get
	LDR R1, =last_input_line_len
	STR R0, [R1]
	POP {R0-R1}

	// Run first-comp + second-comp loops
	PUSH {R0-R5}
	MOV R2, #0 // our index
_collect_first_compartment_priorities:
	@ BL _log_integer
	LDR R0, =last_input_line
	LDRB R0, [R0,R2] // collect item using line pointer + index
	BL _get_item_priority // returned in R0
	@ BL _log_integer
	BL _update_bitset
	ADD R2, R2, #1 // advance index
	LDR R1, =last_input_line_len
	LDR R1, [R1]
	LSR R1, R1, #1
	CMP R2, R1 // check if we've reached half way point
	BLT _collect_first_compartment_priorities
	BL _log_finished_first_compartment
_check_second_compartment_priorities:
	@ BL _log_integer
	LDR R0, =last_input_line
	LDRB R0, [R0,R2] // collect item using line pointer + index
	BL _get_item_priority // returned in R0
	MOV R3, R0
	BL _check_membership_in_bitset
	CMP R0, #0
	MOV R0, R3
	BNE _found_our_common_priority
	ADD R2, R2, #1 // advance index
	LDR R1, =last_input_line_len
	LDR R1, [R1]
	CMP R2, R1 // check if we've reached line end
	BLT _check_second_compartment_priorities
	BL _log_failed_to_find_common_item
	B _finished_handling_line
_found_our_common_priority: // in R0
	LDR R1, =priorities_sum
	LDR R2, [R1]
	BL _log_priorities_increment
	ADD R2, R2, R0
	STR R2, [R1]
_finished_handling_line:
	BL _clear_bitset
	POP {R0-R5}
	B _next_line

_finished:
	PUSH {R0-R1}
	LDR R0, =output_format_priorities_sum
	LDR R1, =priorities_sum
	LDR R1, [R1]
	BL printf
	POP {R0-R1}
	MOV R0, #0 @ success exit code
_exit:
	POP {IP, LR}
 	MOV R7, #1			@ setup registers for syscall exit
 	SWI 0				@ execute syscall exit

_get_item_priority: // item in R0
 	PUSH {R1-R3, LR}
	SUB R1, R0, #'A'
	CMP R1, #('a' - 'A')
	BGE _get_lower_case_item_priority
	ADD R0, R1, #27
	B _finish_getting_item_priority
_get_lower_case_item_priority:
	SUB R0, R1, #('a' - 'A' - 1)
_finish_getting_item_priority:
 	POP {R1-R3, LR}
 	BX LR

_update_bitset: // low, high and new bit index in R0, R1 and R2, respectively
 	PUSH {R0-R3, LR}
	CMP R0, #32
	BGE _update_high_bitset
	LDR R1, =bitset_low
_update_bitset_word:
	LDR R2, [R1]
	MOV R3, #1
	LSL R3, R3, R0 // bitmask
	ORR R2, R2, R3
	STR R2, [R1]
 	POP {R0-R3, LR}
 	BX LR
_update_high_bitset:
	SUB R0, R0, #32
	LDR R1, = bitset_high
	B _update_bitset_word

_check_membership_in_bitset:
 	PUSH {R1-R2, LR}
	CMP R0, #32
	BGE _check_membership_in_high_bitset
	LDR R1, =bitset_low
_check_membership_in_bitset_word:
	LDR R1, [R1]
	MOV R2, #1
	LSL R2, R2, R0 // bitmask
	AND R0, R1, R2
 	POP {R1-R2, LR}
 	BX LR
_check_membership_in_high_bitset:
	SUB R0, R0, #32
	LDR R1, =bitset_high
	B _check_membership_in_bitset_word

_clear_bitset:
	PUSH {R0-R1, LR}
	MOV R0, #0
	LDR R1, =bitset_low
	STR R0, [R1]
	LDR R1, =bitset_high
	STR R0, [R1]
	BX LR

_log_rucksack_scanned:
	PUSH {R0-R2, LR}
 	LDR R0, =stderr
 	LDR R0, [R0]
	LDR R1, =output_format_rucksack_scanned
	LDR R2, =last_input_line
	BL fprintf
	POP {R0-R2, LR}
	BX LR
_log_finished_first_compartment:
	PUSH {R0-R2, LR}
 	LDR R0, =stderr
 	LDR R0, [R0]
	LDR R1, =output_format_finished_first_compartment
	BL fprintf
	POP {R0-R2, LR}
	BX LR
_log_integer:
	PUSH {R0-R2, LR}
	MOV R2, R0
 	LDR R0, =stderr
 	LDR R0, [R0]
	LDR R1, =output_format_pointer
	BL fprintf
	POP {R0-R2, LR}
	BX LR
_log_failed_to_find_common_item:
	PUSH {R0-R2, LR}
 	LDR R0, =stderr
 	LDR R0, [R0]
	LDR R1, =output_format_failed_to_find_common_item
	BL fprintf
	POP {R0-R2, LR}
	BX LR
_log_priorities_increment:
	PUSH {R0-R2, LR}
	MOV R2, R0
 	LDR R0, =stderr
 	LDR R0, [R0]
	LDR R1, =output_format_priorities_increment
	BL fprintf
	POP {R0-R2, LR}
	BX LR

.data
last_input_line: .space 256, 0x0
last_input_line_len: .word 0
bitset_low: .word 0
bitset_high: .word 0
priorities_sum: .word 0
output_format_rucksack_scanned: .asciz "rucksack scanned: %s"
output_format_pointer: .asciz "integer: %d\n"
output_format_finished_first_compartment: .asciz "finished first compartment\n"
output_format_failed_to_find_common_item: .asciz "  failed to find common item\n"
output_format_priorities_increment: .asciz "  priorities +%d\n"
output_format_priorities_sum: .asciz "priorities sum: %d\n"
