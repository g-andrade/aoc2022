@ vim:ft=armv5
		.global _start
		.func _start

/* Raspberry PI ARM assembly */

_start:
	PUSH {IP, LR}
_get_line:
	PUSH {R0-R2}
	LDR R0, =last_input_line
	MOV R1, #(16 - 1) @ size of 'last_input_line' buffer - 1 (how to define constants?)
	LDR R2, =stdin
	LDR R2, [R2]
	BL fgets
	CMP R0, #0
	POP {R0-R2}
	BEQ _finished @ failed to read line
_scan_number:
	PUSH {R0-R2}
	LDR R0, =last_input_line
	LDR R1, =input_format_int
	LDR R2, =last_scanned_number
	BL sscanf
	CMP R0, #1
	POP {R0-R2}
	BNE _group_finished
	BL _log_number_scanned
	LDR R9, =group_count_so_far
	LDR R10, [R9]
	LDR R11, =last_scanned_number
	LDR R11, [R11]
	ADD R10, R11
	STR R10, [R9]
	B _get_line
_group_finished:
	BL _log_group_finished
	LDR R9, =group_count_so_far
	LDR R10, [R9]
	LDR R11, =largest_group_count_so_far_D
	STR R10, [R11]
	BL _sort_largest_group_counts_desc
	MOV R10, #0
	STR R10, [R9]
	B _get_line

_sort_largest_group_counts_desc:
	PUSH {R0-R3, LR}
_sort_largest_group_counts_desc_loop:
	LDR R0, =largest_group_count_so_far_A
	LDR R1, [R0]
	LDR R2, =largest_group_count_so_far_B
	LDR R3, [R2]
	CMP R1, R3
	BLT _swap_group_counts

	LDR R0, =largest_group_count_so_far_B
	LDR R1, [R0]
	LDR R2, =largest_group_count_so_far_C
	LDR R3, [R2]
	CMP R1, R3
	BLT _swap_group_counts

	LDR R0, =largest_group_count_so_far_C
	LDR R1, [R0]
	LDR R2, =largest_group_count_so_far_D
	LDR R3, [R2]
	CMP R1, R3
	BLT _swap_group_counts

	POP {R0-R3, LR}
	BX LR
_swap_group_counts:
	STR R1, [R2]
	STR R3, [R0]
	BL _log_group_count_swaps_were_made
	B _sort_largest_group_counts_desc_loop

_finished:
	PUSH {R0-R3}
	LDR R0, =output_format_finished
	LDR R1, =largest_group_count_so_far_A
	LDR R1, [R1]
	LDR R2, =largest_group_count_so_far_B
	LDR R2, [R2]
	LDR R3, =largest_group_count_so_far_C
	LDR R3, [R3]
	BL printf

	LDR R0, =output_top_counts_summed
	LDR R1, =largest_group_count_so_far_A
	LDR R1, [R1]
	LDR R9, =largest_group_count_so_far_B
	LDR R9, [R9]
	ADD R1, R9
	LDR R9, =largest_group_count_so_far_C
	LDR R9, [R9]
	ADD R1, R9
	BL printf

	POP {R0-R3}
	MOV R0, #0 @ success exit code
_exit:
	POP {IP, LR}
 	MOV R7, #1			@ setup registers for syscall exit
 	SWI 0				@ execute syscall exit
_log_number_scanned:
	PUSH {R0-R2, LR}
 	LDR R0, =stderr
 	LDR R0, [R0]
	LDR R1, =output_format_number_scanned
	LDR R2, =last_scanned_number
	LDR R2, [R2]
	BL fprintf
	POP {R0-R2, LR}
	BX LR
_log_group_finished:
	PUSH {R0-R2, LR}
 	LDR R0, =stderr
 	LDR R0, [R0]
	LDR R1, =output_format_group_finished
	LDR R2, =group_count_so_far
	LDR R2, [R2]
	BL fprintf
	POP {R0-R2, LR}
	BX LR
_log_group_count_swaps_were_made:
	PUSH {R0-R3, LR}
	MOV R2, R1
 	LDR R0, =stderr
 	LDR R0, [R0]
	LDR R1, =output_format_group_count_swaps_were_made
	BL fprintf
	POP {R0-R3, LR}
	BX LR

.data

.balign 4
input_format_int: .string "%d"
output_format_number: .asciz "number: %d\n"
output_format_number_scanned: .asciz "number scanned: %d\n"
output_format_group_finished: .asciz "group finished: %d\n"
output_format_group_count_swaps_were_made: .asciz "group counts were swapped: %d for %d\n"
output_format_finished: .asciz "top 3 largest counts: %d, %d and %d\n"
output_top_counts_summed: .asciz "sum: %d\n"

.balign 4
last_input_line: .space 16, 0x0

last_scanned_number: .word 0
group_count_so_far: .word 0
largest_group_count_so_far_A: .word 0
largest_group_count_so_far_B: .word 0
largest_group_count_so_far_C: .word 0
largest_group_count_so_far_D: .word 0
