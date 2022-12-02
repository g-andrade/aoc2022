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
_react_to_play:
	LDR R2, =last_input_line
	BL _log_play_scanned
	LDRB R0, [R2]
	LDRB R1, [R2,#2]
	BL _calculate_score_increment @ returned in R0
	MOV R2, R0
	BL _log_score_increment
	LDR R3, =total_score
	LDR R4, [R3]
	ADD R2, R4, R2
	STR R2, [R3]
	B _get_line
_finished:
	PUSH {R0-R1}
	LDR R0, =output_format_total_score
	LDR R1, =total_score
	LDR R1, [R1]
	BL printf
	POP {R0-R1}
	MOV R0, #0 @ success exit code
_exit:
	POP {IP, LR}
 	MOV R7, #1			@ setup registers for syscall exit
 	SWI 0				@ execute syscall exit

_calculate_score_increment: @ returned in R0
	PUSH {LR}
	CMP R0, #'A'
	BEQ _opponent_played_rock
	CMP R0, #'B'
	BEQ _opponent_played_paper
	B _opponent_played_scissors
_finished_calculating_score_increment:
	POP {LR}
	BX LR

_opponent_played_rock:
	CMP R1, #'X'
	BEQ _rock_vs_rock
	CMP R1, #'Y'
	BEQ _rock_vs_paper
	B _rock_vs_scissors
_rock_vs_rock:
	MOV R0, #(3 + 1)
	B _finished_calculating_score_increment
_rock_vs_paper:
	MOV R0, #(6 + 2)
	B _finished_calculating_score_increment
_rock_vs_scissors:
	MOV R0, #(0 + 3)
	B _finished_calculating_score_increment

_opponent_played_paper:
	CMP R1, #'X'
	BEQ _paper_vs_rock
	CMP R1, #'Y'
	BEQ _paper_vs_paper
	B _paper_vs_scissors
_paper_vs_rock:
	MOV R0, #(0 + 1)
	B _finished_calculating_score_increment
_paper_vs_paper:
	MOV R0, #(3 + 2)
	B _finished_calculating_score_increment
_paper_vs_scissors:
	MOV R0, #(6 + 3)
	B _finished_calculating_score_increment

_opponent_played_scissors:
	CMP R1, #'X'
	BEQ _scissors_vs_rock
	CMP R1, #'Y'
	BEQ _scissors_vs_paper
	B _scissors_vs_scissors
_scissors_vs_rock:
	MOV R0, #(6 + 1)
	B _finished_calculating_score_increment
_scissors_vs_paper:
	MOV R0, #(0 + 2)
	B _finished_calculating_score_increment
_scissors_vs_scissors:
	MOV R0, #(3 + 3)
	B _finished_calculating_score_increment

_log_play_scanned:
	PUSH {R0-R2, LR}
 	LDR R0, =stderr
 	LDR R0, [R0]
	LDR R1, =output_format_play_scanned
	@ our caller already placed the play in R2
	BL fprintf
	POP {R0-R2, LR}
	BX LR
_log_score_increment:
	PUSH {R0-R2, LR}
 	LDR R0, =stderr
 	LDR R0, [R0]
	LDR R1, =output_format_score_increment
	@ our caller already placed the increment in R2
	BL fprintf
	POP {R0-R2, LR}
	BX LR

.data
last_input_line: .space 16, 0x0
total_score: .word 0
output_format_play_scanned: .asciz "play scanned: %s"
output_format_score_increment: .asciz "  score +%d\n"
output_format_total_score: .asciz "total score: %d\n"
