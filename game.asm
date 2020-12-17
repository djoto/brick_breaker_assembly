;BRICK BREAKER GAME
; ---- Data segment
dseg segment 'DATA'
    WINDOW_WIDTH DW 140h   ; sirina prozora (320 px)
	WINDOW_HEIGHT DW 0C8h  ; visina prozora (200 px)
	WINDOW_BOUNDS DW 0     ; promenljiva pomocu koje mozemo izvrsiti ranu detekciju sudara
	
	TIME_AUX DB 0h ; vremenska promenljiva 

	BALL_X DW 9Eh  ; X pozicija (kolona) lopte  158px
	BALL_Y DW 0BAh ; Y pozicija (kolona) lopte  186px
	BALL_SIZE DW 04h ; velicina lopte (koliko piksela u sirinu i u visinu)
	BALL_VELOCITY_X DW 05h  ; X (horizontalna) komponenta brzine
	BALL_VELOCITY_Y DW -04h ; Y (vertikalna) komponenta brzine (minus je da bi u startu krenula loptica ka gore a ne ka dole) 
	
    BLOCK_SIZE_X DW 36h     ; sirina bloka 54px
	BLOCK_SIZE_Y DW 12h     ; visina bloka 18px

	BLOCK_X_COL_1 DW 08h   ; X pozicija prvog bloka 8px
	BLOCK_X_COL_2 DW 46h   ; X pozicija drugog bloka 70px
    BLOCK_X_COL_3 DW 84h   ; X pozicija treceg bloka 132px
    BLOCK_X_COL_4 DW 0C2h  ; X pozicija cetvrtog bloka 194px
    BLOCK_X_COL_5 DW 0100h ; X pozicija petog bloka 256px
    
    BLOCK_Y_ROW_1 DW 08h   ; Y pozicija prvog reda blokova 8px
	BLOCK_Y_ROW_2 DW 22h   ; Y pozicija drugog reda blokova 34px (8+18+8)
	BLOCK_Y_ROW_3 DW 3Ch   ; Y pozicija treceg reda blokova 60px (8+18+8+18+8) 
	
	; VRIJEDNOSTI IVICA BLOKOVA
	BLOCK_ROW_1_COL_1_TOP DW 08h       ;8px  gornja  y koordinata gornje ivice prvog bloka
	BLOCK_ROW_1_COL_1_BOTTOM DW 1Ah    ;26px (8+18)  y koordinata donje ivice prvog bloka 
	BLOCK_ROW_1_COL_1_RIGHT DW 3Eh     ;62px (8+54)  x koordinata desne ivice prvog bloka
	BLOCK_ROW_1_COL_1_LEFT DW 08h      ;8px          x koordinata lijeve ivice prvog bloka
	COUNTER_BLOCK_ROW_1_COL_1 DW 1h    ;brojac - setovace se na 0 kada lopta prvi put udje u koordinate bloka
	COUNTER_SCORE_ROW_1_COL_1 DW 0h    ;brojac za poene - setovace se na 1 kada lopta dodirne blok prvi put ali prije toga se provjerava da li je
                                       ;vrijednost ovog brojaca 0 (a tada jeste) i tako ce se poeni uracunati samo jednom kad loptica tek dodirne blok,
                                       ;a ne svaki put kad lopta promjeni svoje koordinate unutar koordinata bloka
	
    BLOCK_ROW_1_COL_2_TOP DW 08h
	BLOCK_ROW_1_COL_2_BOTTOM DW 1Ah
	BLOCK_ROW_1_COL_2_RIGHT DW 7Ch     ;124px (8+54+8+54)
	BLOCK_ROW_1_COL_2_LEFT DW 46h      ;70px (8+54+8)
	COUNTER_BLOCK_ROW_1_COL_2 DW 1h
	COUNTER_SCORE_ROW_1_COL_2 DW 0h
	
    BLOCK_ROW_1_COL_3_TOP DW 08h 
	BLOCK_ROW_1_COL_3_BOTTOM DW 1Ah
	BLOCK_ROW_1_COL_3_RIGHT DW 0BAh     ;186px (8+54+8+54+8+54)
	BLOCK_ROW_1_COL_3_LEFT DW 84h      ;132px (8+54+8+54+8)
	COUNTER_BLOCK_ROW_1_COL_3 DW 1h
	COUNTER_SCORE_ROW_1_COL_3 DW 0h
	
    BLOCK_ROW_1_COL_4_TOP DW 08h
	BLOCK_ROW_1_COL_4_BOTTOM DW 1Ah
	BLOCK_ROW_1_COL_4_RIGHT DW 0F8h     ;248px (8+54+8+54+8+54+8+54)
	BLOCK_ROW_1_COL_4_LEFT DW 0C2h      ;194px (8+54+8+54+8+54+8)
	COUNTER_BLOCK_ROW_1_COL_4 DW 1h
	COUNTER_SCORE_ROW_1_COL_4 DW 0h
	
    BLOCK_ROW_1_COL_5_TOP DW 08h 
	BLOCK_ROW_1_COL_5_BOTTOM DW 1Ah
	BLOCK_ROW_1_COL_5_RIGHT DW 136h     ;310px (8+54+8+54+8+54+8+54+8+54)
	BLOCK_ROW_1_COL_5_LEFT DW 100h      ;256px (8+54+8+54+8+54+8+54+8)
	COUNTER_BLOCK_ROW_1_COL_5 DW 1h
	COUNTER_SCORE_ROW_1_COL_5 DW 0h
	
	
    BLOCK_ROW_2_COL_1_TOP DW 22h       ;34px (8+18+8)
	BLOCK_ROW_2_COL_1_BOTTOM DW 34h    ;52px (8+18+8+18)
	BLOCK_ROW_2_COL_1_RIGHT DW 3Eh     ;62px (8+54)
	BLOCK_ROW_2_COL_1_LEFT DW 08h      ;8px
	COUNTER_BLOCK_ROW_2_COL_1 DW 1h
	COUNTER_SCORE_ROW_2_COL_1 DW 0h
	
    BLOCK_ROW_2_COL_2_TOP DW 22h       
	BLOCK_ROW_2_COL_2_BOTTOM DW 34h
	BLOCK_ROW_2_COL_2_RIGHT DW 7Ch     ;124px (8+54+8+54)
	BLOCK_ROW_2_COL_2_LEFT DW 46h      ;70px (8+54+8)
	COUNTER_BLOCK_ROW_2_COL_2 DW 1h
	COUNTER_SCORE_ROW_2_COL_2 DW 0h
	
    BLOCK_ROW_2_COL_3_TOP DW 22h 
	BLOCK_ROW_2_COL_3_BOTTOM DW 34h
	BLOCK_ROW_2_COL_3_RIGHT DW 0BAh     ;186px (8+54+8+54+8+54)
	BLOCK_ROW_2_COL_3_LEFT DW 84h      ;132px (8+54+8+54+8)
	COUNTER_BLOCK_ROW_2_COL_3 DW 1h
	COUNTER_SCORE_ROW_2_COL_3 DW 0h
	
    BLOCK_ROW_2_COL_4_TOP DW 22h
	BLOCK_ROW_2_COL_4_BOTTOM DW 34h
	BLOCK_ROW_2_COL_4_RIGHT DW 0F8h     ;248px (8+54+8+54+8+54+8+54)
	BLOCK_ROW_2_COL_4_LEFT DW 0C2h      ;194px (8+54+8+54+8+54+8)
	COUNTER_BLOCK_ROW_2_COL_4 DW 1h
	COUNTER_SCORE_ROW_2_COL_4 DW 0h
	
    BLOCK_ROW_2_COL_5_TOP DW 22h 
	BLOCK_ROW_2_COL_5_BOTTOM DW 34h
	BLOCK_ROW_2_COL_5_RIGHT DW 136h     ;310px (8+54+8+54+8+54+8+54+8+54)
	BLOCK_ROW_2_COL_5_LEFT DW 100h      ;256px (8+54+8+54+8+54+8+54+8)
	COUNTER_BLOCK_ROW_2_COL_5 DW 1h
	COUNTER_SCORE_ROW_2_COL_5 DW 0h
	
    BLOCK_ROW_3_COL_1_TOP DW 3Ch       ;60px (8+18+8+18+8)
	BLOCK_ROW_3_COL_1_BOTTOM DW 4Eh    ;78px (8+18+8+18+8+18)
	BLOCK_ROW_3_COL_1_RIGHT DW 3Eh     ;62px (8+54)
	BLOCK_ROW_3_COL_1_LEFT DW 08h      ;8px
	COUNTER_BLOCK_ROW_3_COL_1 DW 1h
	COUNTER_SCORE_ROW_3_COL_1 DW 0h
	
    BLOCK_ROW_3_COL_2_TOP DW 3Ch
	BLOCK_ROW_3_COL_2_BOTTOM DW 4Eh
	BLOCK_ROW_3_COL_2_RIGHT DW 7Ch     ;124px (8+54+8+54)
	BLOCK_ROW_3_COL_2_LEFT DW 46h      ;70px (8+54+8)
	COUNTER_BLOCK_ROW_3_COL_2 DW 1h
	COUNTER_SCORE_ROW_3_COL_2 DW 0h
	
    BLOCK_ROW_3_COL_3_TOP DW 3Ch 
	BLOCK_ROW_3_COL_3_BOTTOM DW 4Eh
	BLOCK_ROW_3_COL_3_RIGHT DW 0BAh     ;186px (8+54+8+54+8+54)
	BLOCK_ROW_3_COL_3_LEFT DW 84h      ;132px (8+54+8+54+8)
	COUNTER_BLOCK_ROW_3_COL_3 DW 1h
	COUNTER_SCORE_ROW_3_COL_3 DW 0h
	
    BLOCK_ROW_3_COL_4_TOP DW 3Ch
	BLOCK_ROW_3_COL_4_BOTTOM DW 4Eh
	BLOCK_ROW_3_COL_4_RIGHT DW 0F8h     ;248px (8+54+8+54+8+54+8+54)
	BLOCK_ROW_3_COL_4_LEFT DW 0C2h      ;194px (8+54+8+54+8+54+8)
	COUNTER_BLOCK_ROW_3_COL_4 DW 1h
	COUNTER_SCORE_ROW_3_COL_4 DW 0h
	
    BLOCK_ROW_3_COL_5_TOP DW 3Ch 
	BLOCK_ROW_3_COL_5_BOTTOM DW 4Eh
	BLOCK_ROW_3_COL_5_RIGHT DW 136h     ;310px (8+54+8+54+8+54+8+54+8+54)
	BLOCK_ROW_3_COL_5_LEFT DW 100h      ;256px (8+54+8+54+8+54+8+54+8)
	COUNTER_BLOCK_ROW_3_COL_5 DW 1h
	COUNTER_SCORE_ROW_3_COL_5 DW 0h
	
	NULA DW 0h  ; sluzi za poredjenje sa COUNTER_BLOCK_ROW_X_COL_X vrijednostima
	
    PLATFORM_WIDTH DW 64h  ; sirina platforme 100px
    PLATFORM_HEIGHT DW 06h ; visina platforme 6px
    PLATFORM_X DW 6Eh      ; x koordinata platforme 110px
    PLATFORM_Y DW 0BEh     ; y koordinata platforme 190px
    PLATFORM_VELOCITY_X DW 0Ah ; platforma se pomijera za 10px horizontalno preko tastature
    
    COLOR DB 0Fh
    
    GAME_OVER DB  0AH,0DH,"                                "            ;ispisuje se ako loptica dodirne donju ivicu prozora
              DB  0AH,0DH,"    ================================"
              DB  0AH,0DH,"                                "
              DB  0AH,0DH,"    ================================"
              DB  0AH,0DH,"                                "
              DB  0AH,0DH,"    ========== GAME OVER! =========="
              DB  0AH,0DH,"                                "
              DB  0AH,0DH,"    ================================"
              DB  0AH,0DH,"                                "
              DB  0AH,0DH,"    ==== PRESS `p` TO PLAY AGAIN ==="
              DB  0AH,0DH,"                                "
              DB  0AH,0DH,"    ================================"
              DB  0AH,0DH,"                                "
              DB  0AH,0DH,"    ======= PRESS `e` TO EXIT ======"
              DB  0AH,0DH,"                                "
              DB  0AH,0DH,"    ================================"
              DB  0AH,0DH,"                                "
              DB  0AH,0DH,"                                $"
              
    CONGRATULATIONS_STRING  DB  0AH,0DH,"                                "          ;ispisuje se ako se poruse svi blokovi
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ================================"
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ================================"
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ============ BRAVO! ============"
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ================================"
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ====== SCORE: 30-MAX SCORE ====="
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ================================"
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ==== PRESS `p` TO PLAY AGAIN ==="
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ======= PRESS `e` TO EXIT ======"
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ================================"
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"                                $"
                            
                            
    PLAY_OR_EXIT_STRING     DB  0AH,0DH,"                                "          ;ispisuje kada udjemo u igricu (samo na pocetku)
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ================================"
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ==== PRESS `p` TO PLAY GAME ===="
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ================================"
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ======= PRESS `e` TO EXIT ======"
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"    ================================"
                            DB  0AH,0DH,"                                "
                            DB  0AH,0DH,"                                $"
    
    POMOCNO DW 0h  ;pomocna promjenjiva za ispis poena (ne koristi se nigdje u kodu)
    ON_STACK db "$"  ;sluzi za ispis broja poena
    SCORE DW 0h   ; pocetna vrijednost poena (nula)
    SCORE_PRINT db "score:$" ; string score:
    
dseg ends
; ---- kraj segmenta


; ---- Code segment
cseg	segment	'CODE'
		assume cs:cseg, ds:dseg, ss:sseg
draw:		
	mov ax, dseg
    mov ds, ax
    
    PLAY_OR_EXIT:
        call CLEAR_SCREEN    ;podesi crnu pozadinu
        
        mov ah, 9h                   ;konfiguracija za ispis stringa
        lea dx, PLAY_OR_EXIT_STRING  ;ucitavanje stringa u dx
        int 21h                      ;ispis stringa
        
        mov ah, 0                    ;konfiguracija za slusanje unosa sa tastature
        int 16h                      ;izvrsi konfiguraciju
        cmp ah, 19h                  ;da li je pritisnuto 'p'
        je CHECK_TIME                ;ako jeste skoci na CHECK_TIME tj. kreni sa igrom 
        cmp ah, 12h                  ;da li je pritisnuto 'e'
        je EXIT2                     ;ako jeste skoci na EXIT2 tj. izadji iz igrice
        ret
        
        EXIT2:
        call CLEAR_SCREEN            ;crna pozadina
        jmp kraj                     ;skoci na kraj tj. vrati konzolu
    
	CHECK_TIME:
	
		mov ah,2Ch ; prekid kojim se dobija sistemsko vrijeme
		int 21h    ; CH = sati, CL = minuti, DH = sekunde, DL = stotinke
		
		cmp dl,TIME_AUX  ; da li je trenutno vreme jednako prethodnom (TIME_AUX)?
		je CHECK_TIME    ; ako je isto, proveri ponovo; inace ucrtaj loptu, pomeri je....
		
		mov TIME_AUX,dl ; azuriraj vrijeme
		
		call CLEAR_SCREEN ; obrisi sadrzaj ekrana
		
		call CHECK_BLOCK_ROW_1_COL_1      ;provjerava da li je loptica usla u koordinate bloka
		call DRAW_BLOCK_ROW_1_COL_1_MAIN  ;iscrtace blok ako nije ni jedan put dodirnut lopticom, u suprotnom ne
 		call CHECK_BLOCK_ROW_1_COL_2
 		call DRAW_BLOCK_ROW_1_COL_2_MAIN
        call CHECK_BLOCK_ROW_1_COL_3
        call DRAW_BLOCK_ROW_1_COL_3_MAIN
        call CHECK_BLOCK_ROW_1_COL_4
        call DRAW_BLOCK_ROW_1_COL_4_MAIN
        call CHECK_BLOCK_ROW_1_COL_5
        call DRAW_BLOCK_ROW_1_COL_5_MAIN
 		
		call CHECK_BLOCK_ROW_2_COL_1
		call DRAW_BLOCK_ROW_2_COL_1_MAIN
 		call CHECK_BLOCK_ROW_2_COL_2
 		call DRAW_BLOCK_ROW_2_COL_2_MAIN
        call CHECK_BLOCK_ROW_2_COL_3
        call DRAW_BLOCK_ROW_2_COL_3_MAIN
        call CHECK_BLOCK_ROW_2_COL_4
        call DRAW_BLOCK_ROW_2_COL_4_MAIN
        call CHECK_BLOCK_ROW_2_COL_5
        call DRAW_BLOCK_ROW_2_COL_5_MAIN
 		
		call CHECK_BLOCK_ROW_3_COL_1
		call DRAW_BLOCK_ROW_3_COL_1_MAIN
 		call CHECK_BLOCK_ROW_3_COL_2
 		call DRAW_BLOCK_ROW_3_COL_2_MAIN
        call CHECK_BLOCK_ROW_3_COL_3
        call DRAW_BLOCK_ROW_3_COL_3_MAIN
        call CHECK_BLOCK_ROW_3_COL_4
        call DRAW_BLOCK_ROW_3_COL_4_MAIN
        call CHECK_BLOCK_ROW_3_COL_5
        call DRAW_BLOCK_ROW_3_COL_5_MAIN
		
 		call LISTEN_KEYBOARD   ;pomijera koordinate platforme ako je pritisnuta lijeva ili desna strelica na tastaturi
		
		call DRAW_PLATFORM     ;iscrta platformu
    
		call CHECK_BALL_PLATFORM  ; provjerava da li je loptica dodirnula platformu i u zavisnosti od toga postavlja smjer loptice
		
		call MOVE_BALL ; pomeri loptu
		call DRAW_BALL  ; ucrtaj  je
		
		call CHECK_SCORE  ; provjerava da li je score=30 tj. da li smo presli igricu
		
		call PRINT_SCORE_TOP_RIGHT  ;ispisuje poene u gornjem desnom uglu prozora
		
		jmp CHECK_TIME ; provjeri vreme ponovo
	
	
    PRINT_SCORE_TOP_RIGHT PROC NEAR 

        mov ah, 2h                   
        mov bh, 0h
        mov dl, 98h                 ;odakle pocinje string "score:"
        mov dh, 0h
        int 10h
         
        mov ah, 09h                 ;konfiguracija za ispis
        lea dx, SCORE_PRINT         ;ucitaj string "score:" u dx 
        int 21h                     ;ispisi ga
    
        lea si, ON_STACK            ;za skladistenje poena

        mov ax, SCORE               ;broj poena u ax
        mov bx, 10                  ;u bx ide 10 (ne 10h)
        
        PREPARE_SCORE:
            mov dx,0                
            div bx                  ;ax/10
            add dx, '0'             ;postavljnje u ascii karakter
            dec si                  ;pamtimo karaktere u obrnutom redoslijedu
            mov [si],dl
            cmp ax, 0h            
            jz PRINT                
            jmp PREPARE_SCORE   
            
        PRINT:
            mov ah, 9h 
            mov dx, si
            int 21h
            ret
    PRINT_SCORE_TOP_RIGHT ENDP
	
	CHECK_SCORE PROC NEAR
        mov ax, SCORE
        cmp ax, 1Eh         ;30 poena
        jne NESTO_SCORE
        jmp CONGRATULATIONS
        NESTO_SCORE:
	CHECK_SCORE ENDP
	
	LISTEN_KEYBOARD PROC NEAR
        mov ah, 1           ;slusa unos sa tastature
        int 16h             ;izvrsi konfiguraciju
        jz RETURN           
        mov ah, 0       
        int 16h
        cmp ah, 4Dh         ;da li je pritisnuta desna strelica
        je CHECK_RIGHT      ;ako jeste skoci na labelu za pomjeranje platforme na desno
        cmp ah, 4Bh         ;da li je pritisnuta lijeva strelica
        je CHECK_LEFT       ;ako jeste skoci na labelu za pomijeranje platforme na lijevo
        ret                 
        CHECK_RIGHT:
        mov ax, PLATFORM_X
        add ax, PLATFORM_WIDTH
        cmp ax, 140h                ;da li je PLATFORM_X + PLATFORM_WIDTH = 320px(140h) tj. da li je platforma skroz desno
        jl MOVE_PLATFORM_RIGHT      ;ako je manje pomjeri platformu
        ret
        MOVE_PLATFORM_RIGHT:
        mov ax, PLATFORM_VELOCITY_X
        add PLATFORM_X, ax
        ret
        CHECK_LEFT:
        mov ax, PLATFORM_X
        cmp ax, 0h                  ;da li je PLATFORM_X = 0 tj. da li je platforma skroz lijevo
        jg MOVE_PLATFORM_LEFT       ;ako je vece pomjeri platformu
        ret
        MOVE_PLATFORM_LEFT:
        mov ax, PLATFORM_VELOCITY_X
        sub PLATFORM_X, ax
        ret
        RETURN:
        ret
	LISTEN_KEYBOARD ENDP
	
	
	CHECK_BALL_PLATFORM PROC NEAR
        TOP_PLATFORM:
             mov ax, PLATFORM_Y
             sub ax,BALL_SIZE
             cmp BALL_Y, ax     ;da li je BALL_Y vece od PLATFORM_Y - BALL_SIZE 
             jg RIGHT_PLATFORM  ;ako jeste provjeri da li je BALL_X izvan desne ivice platforme
             ret			
        RIGHT_PLATFORM:
             mov ax, PLATFORM_X
             add ax, PLATFORM_WIDTH
             add ax,BALL_SIZE
             cmp BALL_X, ax     ;da li je BALL_X manje od PLATFORM_X + PLATFORM_WIDTH + BALL_SIZE
             jl LEFT_PLATFORM   ;ako jeste provjeri da li je BALL_X izvan lijeve ivice platforme
             ret
        LEFT_PLATFORM:
             mov ax, PLATFORM_X
             sub ax, BALL_SIZE
             cmp BALL_X, ax     ;da li je BALL_X vece od PLATFORM_X - BALL_SIZE
             jg CHANGE_BALL_WHITE_DIRECTION ;ako jeste skoci na labelu da odbijanje lopte
             ret
        CHANGE_BALL_WHITE_DIRECTION:
             neg BALL_VELOCITY_Y   ;promjena smjera po y osi
             mov al, 0fh           ;stavi kod za bijelu boju u al
             mov COLOR, al         ;upisi kod u COLOR
             ret
    CHECK_BALL_PLATFORM ENDP
    
	
	MOVE_BALL PROC NEAR
		
		mov ax,BALL_VELOCITY_X    
		add BALL_X,ax                   ; pomjeri lopticu horizontalno
		
		mov ax,WINDOW_BOUNDS
		cmp BALL_X,ax                         
		jl NEG_VELOCITY_X_LCYAN         ; BALL_X < 0 + WINDOW_BOUNDS (sudar - lijeva ivica)
		
		mov ax,WINDOW_WIDTH
		sub ax,BALL_SIZE
		sub ax,WINDOW_BOUNDS
		cmp BALL_X,ax	                ;BALL_X > WINDOW_WIDTH - BALL_SIZE  - WINDOW_BOUNDS (sudar - desna ivica)
		jg NEG_VELOCITY_X_MAGENTA
		
		
		mov ax,BALL_VELOCITY_Y
		add BALL_Y,ax                   ; pomjeri lopticu vertikalno
		
		mov ax,WINDOW_BOUNDS
		cmp BALL_Y,ax   		        ;BALL_Y < 0 + WINDOW_BOUNDS (sudar - gornja ivica)
		jl NEG_VELOCITY_Y_BROWN                          
		
		mov ax,WINDOW_HEIGHT	
		sub ax,BALL_SIZE
		sub ax,WINDOW_BOUNDS
		cmp BALL_Y,ax
		jng NESTO_BALL		            ;BALL_Y <= WINDOW_HEIGHT - BALL_SIZE - WINDOW_BOUNDS (sudar - donja ivica)
		jmp KRAJ_IGRE                   ;linija se izvrsava ako gornji uslov nije ispunjen tj. bezuslovno se skace na kraj igre
		NESTO_BALL:
		ret
		NEG_VELOCITY_X_MAGENTA:
			neg BALL_VELOCITY_X   ;BALL_VELOCITY_X = - BALL_VELOCITY_X
            mov al, 05h           ;stavi kod za ljubicastu boju u al
            mov COLOR, al
			ret
        NEG_VELOCITY_X_LCYAN:
			neg BALL_VELOCITY_X   ;BALL_VELOCITY_X = - BALL_VELOCITY_X
            mov al, 0Bh           ;stavi kod za tirkiznu boju u al
            mov COLOR, al
			ret
        NEG_VELOCITY_Y_BROWN:
			neg BALL_VELOCITY_Y  ;BALL_VELOCITY_Y = - BALL_VELOCITY_Y
            mov al, 06h          ;stavi kod za braon boju u al
            mov COLOR, al
			ret
		NEG_VELOCITY_Y:
			neg BALL_VELOCITY_Y   ;BALL_VELOCITY_Y = - BALL_VELOCITY_Y
			ret
		
	MOVE_BALL ENDP
	
	
	DRAW_BALL PROC NEAR
		
		mov cx,BALL_X ; postavi inicijalnu kolonu (X)
		mov dx,BALL_Y ; postavi inicijalni red (Y)
		
		DRAW_BALL_HORIZONTAL:
			mov ah,0Ch ; podesi konfiguraciju za ispis piksela
			mov al, COLOR ; izaberi boju
			mov bh,00h ; 
			int 10h    ; izvrsi konfiguraciju
			
			inc cx     ;cx = cx + 1
			mov ax,cx  
			sub ax,BALL_X ;cx - BALL_X > BALL_SIZE (ako jeste, iscrtali smo za taj red sve kolone; inace nastavljamo dalje)
			cmp ax,BALL_SIZE
			jng DRAW_BALL_HORIZONTAL
			
			mov cx,BALL_X ; vrati cx na inicijalnu kolonu
			inc dx        ; idemo u sledeci red
			
			mov ax,dx    ; dx - BALL_Y > BALL_SIZE (ako jeste, iscrtali smo sve redove piksela; inace nastavljamo dalje)
			sub ax,BALL_Y
			cmp ax,BALL_SIZE
			jng DRAW_BALL_HORIZONTAL
		
		ret
	DRAW_BALL ENDP

	
	CHECK_BLOCK_ROW_1_COL_1 PROC NEAR
        BOTTOM_ROW_1_COL_1:
             mov ax, BLOCK_ROW_1_COL_1_BOTTOM   ;donja ivica bloka u ax
             add ax,BALL_SIZE                   ;ax + BALL_SIZE
             cmp BALL_Y, ax                     ;BALL_Y < BLOCK_ROW_1_COL_1_BOTTOM + BALL_SIZE tj. da li je lopta iznad donje ivice bloka
             jl TOP_ROW_1_COL_1                 ;ako jeste provjeri da li je ispod gornje ivice
             ret
        TOP_ROW_1_COL_1:
             mov ax, BLOCK_ROW_1_COL_1_TOP      ;gornja ivica bloka u ax
             sub ax,BALL_SIZE                   ;ax - BALL_SIZE
             cmp BALL_Y, ax                     ;BALL_Y > BLOCK_ROW_1_COL_1_TOP - BALL_SIZE tj. da li je lopta ispod gornje ivice bloka
             jg RIGHT_ROW_1_COL_1               ;ako jeste provjeri da li je lijevo od desne ivice
             ret			
        RIGHT_ROW_1_COL_1:
             mov ax, BLOCK_ROW_1_COL_1_RIGHT    ;desna ivica u ax
             add ax,BALL_SIZE                   ;ax + BALL_SIZE
             cmp BALL_X, ax                     ;BALL_X < BLOCK_ROW_1_COL_1_RIGHT + BALL_SIZE tj. da li je lopta lijevo od desne ivice
             jl LEFT_ROW_1_COL_1                ;ako jeste provjeri da li je desno od lijeve ivice
             ret
        LEFT_ROW_1_COL_1:
             mov ax, BLOCK_ROW_1_COL_1_LEFT     ;lijeva ivica u ax
             sub ax, BALL_SIZE                  ;ax - BALL_SIZE
             cmp BALL_X, ax                     ;BALL_X > BLOCK_ROW_1_COL_1_LEFT - BALL_SIZE tj. da li je lopta desno od lijeve ivice
             jg COUNT_ROW_1_COL_1               ;ako je i to ispunjeno znaci da su koordinate lopte unutar koordinata bloka i skacemo na labelu za
                                                ;podesavanje broja poena i podesavanje brojaca kako se blok vise ne bi iscrtavao 
             ret
        COUNT_ROW_1_COL_1:
            call CHECK_COUNTER_SCORE_ROW_1_COL_1    ;pozovi f-ju u kojoj provjerava da li je COUNTER_SCORE_ROW_1_COL_1 jos uvijk na nuli
            mov ax, COUNTER_SCORE_ROW_1_COL_1       
            inc ax                                  ;setuje COUNTER_SCORE_ROW_1_COL_1 na jedan prvi put kad udje u COUNT_ROW_1_COL_1 kako se poeni ne bi
                                                    ;racunali vise puta za jedan porusen blok
            mov COUNTER_SCORE_ROW_1_COL_1, ax       
            mov cx, COUNTER_BLOCK_ROW_1_COL_1       
            xor cx, cx                        ;COUNTER_BLOCK_ROW_1_COL_1 se postavlja na 0 kako blok ne bi bio iscrtavan nakon sto je jednom dohvacen                   
            mov COUNTER_BLOCK_ROW_1_COL_1, cx
            ret
    CHECK_BLOCK_ROW_1_COL_1 ENDP
    
    DRAW_BLOCK_ROW_1_COL_1_MAIN PROC NEAR
        LABELA_ROW_1_COL_1:
        mov cx, COUNTER_BLOCK_ROW_1_COL_1
        cmp cx, NULA            ;da li je COUNTER_BLOCK_ROW_1_COL_1 jednak 0
        je NESTO_ROW_1_COL_1    ;ako jeste nula znaci da je blok udaren lopticom i da crtanje bloka treba da se preskoci
        call DRAW_BLOCK_ROW_1_COL_1 ;ako nije 0 (tj. ako je 1) znaci da blok nije udaren lopticom i poziva se f-ja za crtanje bloka
        ret
        NESTO_ROW_1_COL_1:      ;pomocna labela za slucaj da blok udaren i da crtanje bloka treba da se preskoci
        ret
    DRAW_BLOCK_ROW_1_COL_1_MAIN ENDP
    
    CHECK_COUNTER_SCORE_ROW_1_COL_1 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_1_COL_1
        cmp cx, 0h              ;da li je COUNTER_SCORE_ROW_1_COL_1 = 0 tj. da li je blok udaren prvi put
        je ADD_SCORE_ROW_1_COL_1    ;ako jeste skoci na labelu za dodavanje broja poena na vec postojeci broj poena
        ret
        ADD_SCORE_ROW_1_COL_1:
        mov ax, SCORE
        add ax, 3h      ;dodaj vrijednost od 3 poena na prethodni broj poena (3-prvi red (najgornji), 2-drugi red(srednji), 1-treci red(donji))
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_1_COL_1 ENDP
    
    DRAW_BLOCK_ROW_1_COL_1 PROC NEAR    ;f-ja za crtanje bloka (ista logika kao i za crtanje loptice)
        mov cx, BLOCK_X_COL_1
        mov dx, BLOCK_Y_ROW_1
        DRAW_BLOCK_1_1_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 04h ; crvena boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_1
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_1_1_HORIZONTAL
            
            mov cx, BLOCK_X_COL_1;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_1
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_1_1_HORIZONTAL
        ret        
	DRAW_BLOCK_ROW_1_COL_1 ENDP
	
    
    
    CHECK_BLOCK_ROW_1_COL_2 PROC NEAR
        BOTTOM_ROW_1_COL_2:
             mov ax, BLOCK_ROW_1_COL_2_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_1_COL_2 
             ret
        TOP_ROW_1_COL_2:
             mov ax, BLOCK_ROW_1_COL_2_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_1_COL_2 
             ret			
        RIGHT_ROW_1_COL_2:
             mov ax, BLOCK_ROW_1_COL_2_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_1_COL_2
             ret
        LEFT_ROW_1_COL_2:
             mov ax, BLOCK_ROW_1_COL_2_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_1_COL_2
             ret
        COUNT_ROW_1_COL_2:
            call CHECK_COUNTER_SCORE_ROW_1_COL_2
            mov ax, COUNTER_SCORE_ROW_1_COL_2
            inc ax
            mov COUNTER_SCORE_ROW_1_COL_2, ax
            mov cx, COUNTER_BLOCK_ROW_1_COL_2
            xor cx, cx
            mov COUNTER_BLOCK_ROW_1_COL_2, cx
            ret
    CHECK_BLOCK_ROW_1_COL_2 ENDP
    
    DRAW_BLOCK_ROW_1_COL_2_MAIN PROC NEAR
        LABELA_ROW_1_COL_2:
        mov cx, COUNTER_BLOCK_ROW_1_COL_2
        cmp cx, NULA
        je NESTO_ROW_1_COL_2
        call DRAW_BLOCK_ROW_1_COL_2
        ret
        NESTO_ROW_1_COL_2:
        ret
    DRAW_BLOCK_ROW_1_COL_2_MAIN ENDP
    
    CHECK_COUNTER_SCORE_ROW_1_COL_2 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_1_COL_2
        cmp cx, 0h
        je ADD_SCORE_ROW_1_COL_2
        ret
        ADD_SCORE_ROW_1_COL_2:
        mov ax, SCORE
        add ax, 3h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_1_COL_2 ENDP
    
	DRAW_BLOCK_ROW_1_COL_2 PROC NEAR
        mov cx, BLOCK_X_COL_2
        mov dx, BLOCK_Y_ROW_1
        DRAW_BLOCK_1_2_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 04h ; crvena boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_2
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_1_2_HORIZONTAL
            
            mov cx, BLOCK_X_COL_2;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_1
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_1_2_HORIZONTAL
        ret
	DRAW_BLOCK_ROW_1_COL_2 ENDP
	
	
	
    CHECK_BLOCK_ROW_1_COL_3 PROC NEAR
        BOTTOM_ROW_1_COL_3:
             mov ax, BLOCK_ROW_1_COL_3_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_1_COL_3 
             ret
        TOP_ROW_1_COL_3:
             mov ax, BLOCK_ROW_1_COL_3_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_1_COL_3
             ret			
        RIGHT_ROW_1_COL_3:
             mov ax, BLOCK_ROW_1_COL_3_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_1_COL_3
             ret
        LEFT_ROW_1_COL_3:
             mov ax, BLOCK_ROW_1_COL_3_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_1_COL_3
             ret
        COUNT_ROW_1_COL_3:
            call CHECK_COUNTER_SCORE_ROW_1_COL_3
            mov ax, COUNTER_SCORE_ROW_1_COL_3
            inc ax
            mov COUNTER_SCORE_ROW_1_COL_3, ax
            mov cx, COUNTER_BLOCK_ROW_1_COL_3
            xor cx, cx
            mov COUNTER_BLOCK_ROW_1_COL_3, cx
            ret
    CHECK_BLOCK_ROW_1_COL_3 ENDP
    
    DRAW_BLOCK_ROW_1_COL_3_MAIN PROC NEAR
        LABELA_ROW_1_COL_3:
        mov cx, COUNTER_BLOCK_ROW_1_COL_3
        cmp cx, NULA
        je NESTO_ROW_1_COL_3
        call DRAW_BLOCK_ROW_1_COL_3
        ret
        NESTO_ROW_1_COL_3:
        ret
    DRAW_BLOCK_ROW_1_COL_3_MAIN ENDP
    
    CHECK_COUNTER_SCORE_ROW_1_COL_3 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_1_COL_3
        cmp cx, 0h
        je ADD_SCORE_ROW_1_COL_3
        ret
        ADD_SCORE_ROW_1_COL_3:
        mov ax, SCORE
        add ax, 3h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_1_COL_3 ENDP
    
	DRAW_BLOCK_ROW_1_COL_3 PROC NEAR
        mov cx, BLOCK_X_COL_3
        mov dx, BLOCK_Y_ROW_1
        DRAW_BLOCK_1_3_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 04h ; crvena boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_3
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_1_3_HORIZONTAL
            
            mov cx, BLOCK_X_COL_3;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_1
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_1_3_HORIZONTAL
        ret
	DRAW_BLOCK_ROW_1_COL_3 ENDP
	
	
    CHECK_BLOCK_ROW_1_COL_4 PROC NEAR
        BOTTOM_ROW_1_COL_4:
             mov ax, BLOCK_ROW_1_COL_4_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_1_COL_4
             ret
        TOP_ROW_1_COL_4:
             mov ax, BLOCK_ROW_1_COL_4_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_1_COL_4
             ret			
        RIGHT_ROW_1_COL_4:
             mov ax, BLOCK_ROW_1_COL_4_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_1_COL_4
             ret
        LEFT_ROW_1_COL_4:
             mov ax, BLOCK_ROW_1_COL_4_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_1_COL_4
             ret
        COUNT_ROW_1_COL_4:
            call CHECK_COUNTER_SCORE_ROW_1_COL_4
            mov ax, COUNTER_SCORE_ROW_1_COL_4
            inc ax
            mov COUNTER_SCORE_ROW_1_COL_4, ax
            mov cx, COUNTER_BLOCK_ROW_1_COL_4
            xor cx, cx
            mov COUNTER_BLOCK_ROW_1_COL_4, cx
            ret
    CHECK_BLOCK_ROW_1_COL_4 ENDP
    
    DRAW_BLOCK_ROW_1_COL_4_MAIN PROC NEAR
        LABELA_ROW_1_COL_4:
        mov cx, COUNTER_BLOCK_ROW_1_COL_4
        cmp cx, NULA
        je NESTO_ROW_1_COL_4
        call DRAW_BLOCK_ROW_1_COL_4
        ret
        NESTO_ROW_1_COL_4:
        ret
    DRAW_BLOCK_ROW_1_COL_4_MAIN ENDP
    
    CHECK_COUNTER_SCORE_ROW_1_COL_4 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_1_COL_4
        cmp cx, 0h
        je ADD_SCORE_ROW_1_COL_4
        ret
        ADD_SCORE_ROW_1_COL_4:
        mov ax, SCORE
        add ax, 3h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_1_COL_4 ENDP
    
	DRAW_BLOCK_ROW_1_COL_4 PROC NEAR
        mov cx, BLOCK_X_COL_4
        mov dx, BLOCK_Y_ROW_1
        DRAW_BLOCK_1_4_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 04h ; crvena boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_4
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_1_4_HORIZONTAL
            
            mov cx, BLOCK_X_COL_4;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_1
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_1_4_HORIZONTAL  
        ret
	DRAW_BLOCK_ROW_1_COL_4 ENDP
	
	
	
    CHECK_BLOCK_ROW_1_COL_5 PROC NEAR
        BOTTOM_ROW_1_COL_5:
             mov ax, BLOCK_ROW_1_COL_5_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_1_COL_5 
             ret
        TOP_ROW_1_COL_5:
             mov ax, BLOCK_ROW_1_COL_5_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_1_COL_5
             ret			
        RIGHT_ROW_1_COL_5:
             mov ax, BLOCK_ROW_1_COL_5_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_1_COL_5
             ret
        LEFT_ROW_1_COL_5:
             mov ax, BLOCK_ROW_1_COL_5_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_1_COL_5
             ret
        COUNT_ROW_1_COL_5:
            call CHECK_COUNTER_SCORE_ROW_1_COL_5
            mov ax, COUNTER_SCORE_ROW_1_COL_5
            inc ax
            mov COUNTER_SCORE_ROW_1_COL_5, ax
            mov cx, COUNTER_BLOCK_ROW_1_COL_5
            xor cx, cx
            mov COUNTER_BLOCK_ROW_1_COL_5, cx
            ret
    CHECK_BLOCK_ROW_1_COL_5 ENDP
    
    DRAW_BLOCK_ROW_1_COL_5_MAIN PROC NEAR
        LABELA_ROW_1_COL_5:
        mov cx, COUNTER_BLOCK_ROW_1_COL_5
        cmp cx, NULA
        je NESTO_ROW_1_COL_5
        call DRAW_BLOCK_ROW_1_COL_5
        ret
        NESTO_ROW_1_COL_5:
        ret
    DRAW_BLOCK_ROW_1_COL_5_MAIN ENDP
    
    CHECK_COUNTER_SCORE_ROW_1_COL_5 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_1_COL_5
        cmp cx, 0h
        je ADD_SCORE_ROW_1_COL_5
        ret
        ADD_SCORE_ROW_1_COL_5:
        mov ax, SCORE
        add ax, 3h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_1_COL_5 ENDP
    
	DRAW_BLOCK_ROW_1_COL_5 PROC NEAR
        mov cx, BLOCK_X_COL_5
        mov dx, BLOCK_Y_ROW_1        
        DRAW_BLOCK_1_5_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 04h ; crvena boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_5
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_1_5_HORIZONTAL
            
            mov cx, BLOCK_X_COL_5;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_1
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_1_5_HORIZONTAL            
        ret
	DRAW_BLOCK_ROW_1_COL_5 ENDP
	
	
    CHECK_BLOCK_ROW_2_COL_1 PROC NEAR
        BOTTOM_ROW_2_COL_1:
             mov ax, BLOCK_ROW_2_COL_1_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_2_COL_1 
             ret
        TOP_ROW_2_COL_1:
             mov ax, BLOCK_ROW_2_COL_1_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_2_COL_1 
             ret			
        RIGHT_ROW_2_COL_1:
             mov ax, BLOCK_ROW_2_COL_1_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_2_COL_1
             ret
        LEFT_ROW_2_COL_1:
             mov ax, BLOCK_ROW_2_COL_1_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_2_COL_1
             ret
        COUNT_ROW_2_COL_1:
            call CHECK_COUNTER_SCORE_ROW_2_COL_1
            mov ax, COUNTER_SCORE_ROW_2_COL_1
            inc ax
            mov COUNTER_SCORE_ROW_2_COL_1, ax
            mov cx, COUNTER_BLOCK_ROW_2_COL_1
            xor cx, cx
            mov COUNTER_BLOCK_ROW_2_COL_1, cx
            ret
    CHECK_BLOCK_ROW_2_COL_1 ENDP
    
    DRAW_BLOCK_ROW_2_COL_1_MAIN PROC NEAR
        LABELA_ROW_2_COL_1:
        mov cx, COUNTER_BLOCK_ROW_2_COL_1
        cmp cx, NULA
        je NESTO_ROW_2_COL_1
        call DRAW_BLOCK_ROW_2_COL_1
        ret
        NESTO_ROW_2_COL_1:
        ret
    DRAW_BLOCK_ROW_2_COL_1_MAIN ENDP
	
    CHECK_COUNTER_SCORE_ROW_2_COL_1 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_2_COL_1
        cmp cx, 0h
        je ADD_SCORE_ROW_2_COL_1
        ret
        ADD_SCORE_ROW_2_COL_1:
        mov ax, SCORE
        add ax, 2h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_2_COL_1 ENDP
	
	DRAW_BLOCK_ROW_2_COL_1 PROC NEAR
        mov cx, BLOCK_X_COL_1
        mov dx, BLOCK_Y_ROW_2
        DRAW_BLOCK_2_1_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 0Eh ; zuta boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_1
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_2_1_HORIZONTAL
            
            mov cx, BLOCK_X_COL_1;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_2
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_2_1_HORIZONTAL  
        ret
    DRAW_BLOCK_ROW_2_COL_1 ENDP
    
    
    CHECK_BLOCK_ROW_2_COL_2 PROC NEAR
        BOTTOM_ROW_2_COL_2:
             mov ax, BLOCK_ROW_2_COL_2_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_2_COL_2 
             ret
        TOP_ROW_2_COL_2:
             mov ax, BLOCK_ROW_2_COL_2_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_2_COL_2 
             ret			
        RIGHT_ROW_2_COL_2:
             mov ax, BLOCK_ROW_2_COL_2_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_2_COL_2
             ret
        LEFT_ROW_2_COL_2:
             mov ax, BLOCK_ROW_2_COL_2_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_2_COL_2
             ret
        COUNT_ROW_2_COL_2:
            call CHECK_COUNTER_SCORE_ROW_2_COL_2
            mov ax, COUNTER_SCORE_ROW_2_COL_2
            inc ax
            mov COUNTER_SCORE_ROW_2_COL_2, ax
            mov cx, COUNTER_BLOCK_ROW_2_COL_2
            xor cx, cx
            mov COUNTER_BLOCK_ROW_2_COL_2, cx
            ret
    CHECK_BLOCK_ROW_2_COL_2 ENDP
    
    DRAW_BLOCK_ROW_2_COL_2_MAIN PROC NEAR
        LABELA_ROW_2_COL_2:
        mov cx, COUNTER_BLOCK_ROW_2_COL_2
        cmp cx, NULA
        je NESTO_ROW_2_COL_2
        call DRAW_BLOCK_ROW_2_COL_2
        ret
        NESTO_ROW_2_COL_2:
        ret
    DRAW_BLOCK_ROW_2_COL_2_MAIN ENDP
    
    CHECK_COUNTER_SCORE_ROW_2_COL_2 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_2_COL_2
        cmp cx, 0h
        je ADD_SCORE_ROW_2_COL_2
        ret
        ADD_SCORE_ROW_2_COL_2:
        mov ax, SCORE
        add ax, 2h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_2_COL_2 ENDP
    
    DRAW_BLOCK_ROW_2_COL_2 PROC NEAR        
        mov cx, BLOCK_X_COL_2
        mov dx, BLOCK_Y_ROW_2        
        DRAW_BLOCK_2_2_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 0Eh ; zuta boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_2
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_2_2_HORIZONTAL
            
            mov cx, BLOCK_X_COL_2
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_2
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_2_2_HORIZONTAL
        ret
    DRAW_BLOCK_ROW_2_COL_2 ENDP
    
    
    CHECK_BLOCK_ROW_2_COL_3 PROC NEAR
        BOTTOM_ROW_2_COL_3:
             mov ax, BLOCK_ROW_2_COL_3_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_2_COL_3 
             ret
        TOP_ROW_2_COL_3:
             mov ax, BLOCK_ROW_2_COL_3_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_2_COL_3
             ret			
        RIGHT_ROW_2_COL_3:
             mov ax, BLOCK_ROW_2_COL_3_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_2_COL_3
             ret
        LEFT_ROW_2_COL_3:
             mov ax, BLOCK_ROW_2_COL_3_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_2_COL_3
             ret
        COUNT_ROW_2_COL_3:
            call CHECK_COUNTER_SCORE_ROW_2_COL_3
            mov ax, COUNTER_SCORE_ROW_2_COL_3
            inc ax
            mov COUNTER_SCORE_ROW_2_COL_3, ax
            mov cx, COUNTER_BLOCK_ROW_2_COL_3
            xor cx, cx
            mov COUNTER_BLOCK_ROW_2_COL_3, cx
            ret
    CHECK_BLOCK_ROW_2_COL_3 ENDP
    
    DRAW_BLOCK_ROW_2_COL_3_MAIN PROC NEAR
        LABELA_ROW_2_COL_3:
        mov cx, COUNTER_BLOCK_ROW_2_COL_3
        cmp cx, NULA
        je NESTO_ROW_2_COL_3
        call DRAW_BLOCK_ROW_2_COL_3
        ret
        NESTO_ROW_2_COL_3:
        ret
    DRAW_BLOCK_ROW_2_COL_3_MAIN ENDP
    
    CHECK_COUNTER_SCORE_ROW_2_COL_3 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_2_COL_3
        cmp cx, 0h
        je ADD_SCORE_ROW_2_COL_3
        ret
        ADD_SCORE_ROW_2_COL_3:
        mov ax, SCORE
        add ax, 2h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_2_COL_3 ENDP
    
    DRAW_BLOCK_ROW_2_COL_3 PROC NEAR
        mov cx, BLOCK_X_COL_3
        mov dx, BLOCK_Y_ROW_2        
        DRAW_BLOCK_2_3_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 0Eh ; zuta boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_3
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_2_3_HORIZONTAL
            
            mov cx, BLOCK_X_COL_3;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_2
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_2_3_HORIZONTAL  
        ret
    DRAW_BLOCK_ROW_2_COL_3 ENDP

    
    CHECK_BLOCK_ROW_2_COL_4 PROC NEAR
        BOTTOM_ROW_2_COL_4:
             mov ax, BLOCK_ROW_2_COL_4_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_2_COL_4 
             ret
        TOP_ROW_2_COL_4:
             mov ax, BLOCK_ROW_2_COL_4_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_2_COL_4
             ret			
        RIGHT_ROW_2_COL_4:
             mov ax, BLOCK_ROW_2_COL_4_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_2_COL_4
             ret
        LEFT_ROW_2_COL_4:
             mov ax, BLOCK_ROW_2_COL_4_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_2_COL_4
             ret
        COUNT_ROW_2_COL_4:
            call CHECK_COUNTER_SCORE_ROW_2_COL_4
            mov ax, COUNTER_SCORE_ROW_2_COL_4
            inc ax
            mov COUNTER_SCORE_ROW_2_COL_4, ax
            mov cx, COUNTER_BLOCK_ROW_2_COL_4
            xor cx, cx
            mov COUNTER_BLOCK_ROW_2_COL_4, cx
            ret
    CHECK_BLOCK_ROW_2_COL_4 ENDP
    
    DRAW_BLOCK_ROW_2_COL_4_MAIN PROC NEAR
        LABELA_ROW_2_COL_4:
        mov cx, COUNTER_BLOCK_ROW_2_COL_4
        cmp cx, NULA
        je NESTO_ROW_2_COL_4
        call DRAW_BLOCK_ROW_2_COL_4
        ret
        NESTO_ROW_2_COL_4:
        ret
    DRAW_BLOCK_ROW_2_COL_4_MAIN ENDP
    
    CHECK_COUNTER_SCORE_ROW_2_COL_4 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_2_COL_4
        cmp cx, 0h
        je ADD_SCORE_ROW_2_COL_4
        ret
        ADD_SCORE_ROW_2_COL_4:
        mov ax, SCORE
        add ax, 2h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_2_COL_4 ENDP
    
    DRAW_BLOCK_ROW_2_COL_4 PROC NEAR
        mov cx, BLOCK_X_COL_4
        mov dx, BLOCK_Y_ROW_2        
        DRAW_BLOCK_2_4_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 0Eh ; zuta boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_4
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_2_4_HORIZONTAL
            
            mov cx, BLOCK_X_COL_4;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_2
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_2_4_HORIZONTAL  
        ret
    DRAW_BLOCK_ROW_2_COL_4 ENDP
    
    
    CHECK_BLOCK_ROW_2_COL_5 PROC NEAR
        BOTTOM_ROW_2_COL_5:
             mov ax, BLOCK_ROW_2_COL_5_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_2_COL_5 
             ret
        TOP_ROW_2_COL_5:
             mov ax, BLOCK_ROW_2_COL_5_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_2_COL_5
             ret			
        RIGHT_ROW_2_COL_5:
             mov ax, BLOCK_ROW_2_COL_5_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_2_COL_5
             ret
        LEFT_ROW_2_COL_5:
             mov ax, BLOCK_ROW_2_COL_5_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_2_COL_5
             ret
        COUNT_ROW_2_COL_5:
            call CHECK_COUNTER_SCORE_ROW_2_COL_5
            mov ax, COUNTER_SCORE_ROW_2_COL_5
            inc ax
            mov COUNTER_SCORE_ROW_2_COL_5, ax
            mov cx, COUNTER_BLOCK_ROW_2_COL_5
            xor cx, cx
            mov COUNTER_BLOCK_ROW_2_COL_5, cx
            ret
    CHECK_BLOCK_ROW_2_COL_5 ENDP
    
    DRAW_BLOCK_ROW_2_COL_5_MAIN PROC NEAR
        LABELA_ROW_2_COL_5:
        mov cx, COUNTER_BLOCK_ROW_2_COL_5
        cmp cx, NULA
        je NESTO_ROW_2_COL_5
        call DRAW_BLOCK_ROW_2_COL_5
        ret
        NESTO_ROW_2_COL_5:
        ret
    DRAW_BLOCK_ROW_2_COL_5_MAIN ENDP
    
    CHECK_COUNTER_SCORE_ROW_2_COL_5 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_2_COL_5
        cmp cx, 0h
        je ADD_SCORE_ROW_2_COL_5
        ret
        ADD_SCORE_ROW_2_COL_5:
        mov ax, SCORE
        add ax, 2h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_2_COL_5 ENDP
    
    DRAW_BLOCK_ROW_2_COL_5 PROC NEAR
        mov cx, BLOCK_X_COL_5
        mov dx, BLOCK_Y_ROW_2
        DRAW_BLOCK_2_5_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 0Eh ; zuta boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_5
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_2_5_HORIZONTAL
            
            mov cx, BLOCK_X_COL_5;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_2
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_2_5_HORIZONTAL  
        ret
    DRAW_BLOCK_ROW_2_COL_5 ENDP

    
    CHECK_BLOCK_ROW_3_COL_1 PROC NEAR
        BOTTOM_ROW_3_COL_1:
             mov ax, BLOCK_ROW_3_COL_1_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_3_COL_1 
             ret
        TOP_ROW_3_COL_1:
             mov ax, BLOCK_ROW_3_COL_1_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_3_COL_1 
             ret			
        RIGHT_ROW_3_COL_1:
             mov ax, BLOCK_ROW_3_COL_1_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_3_COL_1
             ret
        LEFT_ROW_3_COL_1:
             mov ax, BLOCK_ROW_3_COL_1_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_3_COL_1
             ret
        COUNT_ROW_3_COL_1:
            call CHECK_COUNTER_SCORE_ROW_3_COL_1
            mov ax, COUNTER_SCORE_ROW_3_COL_1
            inc ax
            mov COUNTER_SCORE_ROW_3_COL_1, ax
            mov cx, COUNTER_BLOCK_ROW_3_COL_1
            xor cx, cx
            mov COUNTER_BLOCK_ROW_3_COL_1, cx
            ret
    CHECK_BLOCK_ROW_3_COL_1 ENDP
    
    DRAW_BLOCK_ROW_3_COL_1_MAIN PROC NEAR
        LABELA_ROW_3_COL_1:
        mov cx, COUNTER_BLOCK_ROW_3_COL_1
        cmp cx, NULA
        je NESTO_ROW_3_COL_1
        call DRAW_BLOCK_ROW_3_COL_1
        ret
        NESTO_ROW_3_COL_1:
        ret
    DRAW_BLOCK_ROW_3_COL_1_MAIN ENDP
    
    CHECK_COUNTER_SCORE_ROW_3_COL_1 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_3_COL_1
        cmp cx, 0h
        je ADD_SCORE_ROW_3_COL_1
        ret
        ADD_SCORE_ROW_3_COL_1:
        mov ax, SCORE
        add ax, 1h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_3_COL_1 ENDP
    
    DRAW_BLOCK_ROW_3_COL_1 PROC NEAR
        mov cx, BLOCK_X_COL_1
        mov dx, BLOCK_Y_ROW_3
        DRAW_BLOCK_3_1_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 02h ; zelena boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_1
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_3_1_HORIZONTAL
            
            mov cx, BLOCK_X_COL_1;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_3
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_3_1_HORIZONTAL
        ret
	DRAW_BLOCK_ROW_3_COL_1 ENDP
	
	
    CHECK_BLOCK_ROW_3_COL_2 PROC NEAR
        BOTTOM_ROW_3_COL_2:
             mov ax, BLOCK_ROW_3_COL_2_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_3_COL_2 
             ret
        TOP_ROW_3_COL_2:
             mov ax, BLOCK_ROW_3_COL_2_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_3_COL_2
             ret			
        RIGHT_ROW_3_COL_2:
             mov ax, BLOCK_ROW_3_COL_2_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_3_COL_2
             ret
        LEFT_ROW_3_COL_2:
             mov ax, BLOCK_ROW_3_COL_2_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_3_COL_2
             ret
        COUNT_ROW_3_COL_2:
            call CHECK_COUNTER_SCORE_ROW_3_COL_2
            mov ax, COUNTER_SCORE_ROW_3_COL_2
            inc ax
            mov COUNTER_SCORE_ROW_3_COL_2, ax
            mov cx, COUNTER_BLOCK_ROW_3_COL_2
            xor cx, cx
            mov COUNTER_BLOCK_ROW_3_COL_2, cx
            ret
    CHECK_BLOCK_ROW_3_COL_2 ENDP
    
    DRAW_BLOCK_ROW_3_COL_2_MAIN PROC NEAR
        LABELA_ROW_3_COL_2:
        mov cx, COUNTER_BLOCK_ROW_3_COL_2
        cmp cx, NULA
        je NESTO_ROW_3_COL_2
        call DRAW_BLOCK_ROW_3_COL_2
        ret
        NESTO_ROW_3_COL_2:
        ret
    DRAW_BLOCK_ROW_3_COL_2_MAIN ENDP
	
    CHECK_COUNTER_SCORE_ROW_3_COL_2 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_3_COL_2
        cmp cx, 0h
        je ADD_SCORE_ROW_3_COL_2
        ret
        ADD_SCORE_ROW_3_COL_2:
        mov ax, SCORE
        add ax, 1h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_3_COL_2 ENDP
	
    DRAW_BLOCK_ROW_3_COL_2 PROC NEAR
        mov cx, BLOCK_X_COL_2
        mov dx, BLOCK_Y_ROW_3
        
        DRAW_BLOCK_3_2_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 02h ; zelena boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_2
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_3_2_HORIZONTAL
            
            mov cx, BLOCK_X_COL_2;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_3
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_3_2_HORIZONTAL
        ret
	DRAW_BLOCK_ROW_3_COL_2 ENDP
	
	
    CHECK_BLOCK_ROW_3_COL_3 PROC NEAR
        BOTTOM_ROW_3_COL_3:
             mov ax, BLOCK_ROW_3_COL_3_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_3_COL_3
             ret
        TOP_ROW_3_COL_3:
             mov ax, BLOCK_ROW_3_COL_3_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_3_COL_3
             ret			
        RIGHT_ROW_3_COL_3:
             mov ax, BLOCK_ROW_3_COL_3_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_3_COL_3
             ret
        LEFT_ROW_3_COL_3:
             mov ax, BLOCK_ROW_3_COL_3_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_3_COL_3
             ret
        COUNT_ROW_3_COL_3:
            call CHECK_COUNTER_SCORE_ROW_3_COL_3
            mov ax, COUNTER_SCORE_ROW_3_COL_3
            inc ax
            mov COUNTER_SCORE_ROW_3_COL_3, ax
            mov cx, COUNTER_BLOCK_ROW_3_COL_3
            xor cx, cx
            mov COUNTER_BLOCK_ROW_3_COL_3, cx
            ret
    CHECK_BLOCK_ROW_3_COL_3 ENDP
    
    DRAW_BLOCK_ROW_3_COL_3_MAIN PROC NEAR
        LABELA_ROW_3_COL_3:
        mov cx, COUNTER_BLOCK_ROW_3_COL_3
        cmp cx, NULA
        je NESTO_ROW_3_COL_3
        call DRAW_BLOCK_ROW_3_COL_3
        ret
        NESTO_ROW_3_COL_3:
        ret
    DRAW_BLOCK_ROW_3_COL_3_MAIN ENDP
	
    CHECK_COUNTER_SCORE_ROW_3_COL_3 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_3_COL_3
        cmp cx, 0h
        je ADD_SCORE_ROW_3_COL_3
        ret
        ADD_SCORE_ROW_3_COL_3:
        mov ax, SCORE
        add ax, 1h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_3_COL_3 ENDP
	
	DRAW_BLOCK_ROW_3_COL_3 PROC NEAR
        mov cx, BLOCK_X_COL_3
        mov dx, BLOCK_Y_ROW_3
        DRAW_BLOCK_3_3_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 02h ; zelena boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_3
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_3_3_HORIZONTAL
            
            mov cx, BLOCK_X_COL_3;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_3
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_3_3_HORIZONTAL
        ret
	DRAW_BLOCK_ROW_3_COL_3 ENDP
	
	
    CHECK_BLOCK_ROW_3_COL_4 PROC NEAR
        BOTTOM_ROW_3_COL_4:
             mov ax, BLOCK_ROW_3_COL_4_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_3_COL_4
             ret
        TOP_ROW_3_COL_4:
             mov ax, BLOCK_ROW_3_COL_4_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_3_COL_4
             ret			
        RIGHT_ROW_3_COL_4:
             mov ax, BLOCK_ROW_3_COL_4_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_3_COL_4
             ret
        LEFT_ROW_3_COL_4:
             mov ax, BLOCK_ROW_3_COL_4_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_3_COL_4
             ret
        COUNT_ROW_3_COL_4:
            call CHECK_COUNTER_SCORE_ROW_3_COL_4
            mov ax, COUNTER_SCORE_ROW_3_COL_4
            inc ax
            mov COUNTER_SCORE_ROW_3_COL_4, ax
            mov cx, COUNTER_BLOCK_ROW_3_COL_4
            xor cx, cx
            mov COUNTER_BLOCK_ROW_3_COL_4, cx
            ret
    CHECK_BLOCK_ROW_3_COL_4 ENDP
    
    DRAW_BLOCK_ROW_3_COL_4_MAIN PROC NEAR
        LABELA_ROW_3_COL_4:
        mov cx, COUNTER_BLOCK_ROW_3_COL_4
        cmp cx, NULA
        je NESTO_ROW_3_COL_4
        call DRAW_BLOCK_ROW_3_COL_4
        ret
        NESTO_ROW_3_COL_4:
        ret
    DRAW_BLOCK_ROW_3_COL_4_MAIN ENDP
	
    CHECK_COUNTER_SCORE_ROW_3_COL_4 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_3_COL_4
        cmp cx, 0h
        je ADD_SCORE_ROW_3_COL_4
        ret
        ADD_SCORE_ROW_3_COL_4:
        mov ax, SCORE
        add ax, 1h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_3_COL_4 ENDP
	
	DRAW_BLOCK_ROW_3_COL_4 PROC NEAR
        mov cx, BLOCK_X_COL_4
        mov dx, BLOCK_Y_ROW_3
        DRAW_BLOCK_3_4_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 02h ; zelena boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_4
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_3_4_HORIZONTAL
            
            mov cx, BLOCK_X_COL_4;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_3
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_3_4_HORIZONTAL
        ret
	DRAW_BLOCK_ROW_3_COL_4 ENDP
	
	
    CHECK_BLOCK_ROW_3_COL_5 PROC NEAR
        BOTTOM_ROW_3_COL_5:
             mov ax, BLOCK_ROW_3_COL_5_BOTTOM
             add ax,BALL_SIZE
             cmp BALL_Y, ax
             jl TOP_ROW_3_COL_5 
             ret
        TOP_ROW_3_COL_5:
             mov ax, BLOCK_ROW_3_COL_5_TOP
             sub ax,BALL_SIZE
             cmp BALL_Y, ax
             jg RIGHT_ROW_3_COL_5 
             ret			
        RIGHT_ROW_3_COL_5:
             mov ax, BLOCK_ROW_3_COL_5_RIGHT
             add ax,BALL_SIZE
             cmp BALL_X, ax
             jl LEFT_ROW_3_COL_5
             ret
        LEFT_ROW_3_COL_5:
             mov ax, BLOCK_ROW_3_COL_5_LEFT
             sub ax, BALL_SIZE
             cmp BALL_X, ax
             jg COUNT_ROW_3_COL_5
             ret
        COUNT_ROW_3_COL_5:
            call CHECK_COUNTER_SCORE_ROW_3_COL_5
            mov ax, COUNTER_SCORE_ROW_3_COL_5
            inc ax
            mov COUNTER_SCORE_ROW_3_COL_5, ax
            mov cx, COUNTER_BLOCK_ROW_3_COL_5
            xor cx, cx
            mov COUNTER_BLOCK_ROW_3_COL_5, cx
            ret
    CHECK_BLOCK_ROW_3_COL_5 ENDP
    
    DRAW_BLOCK_ROW_3_COL_5_MAIN PROC NEAR
        LABELA_ROW_3_COL_5:
        mov cx, COUNTER_BLOCK_ROW_3_COL_5
        cmp cx, NULA
        je NESTO_ROW_3_COL_5
        call DRAW_BLOCK_ROW_3_COL_5
        ret
        NESTO_ROW_3_COL_5:
        ret
    DRAW_BLOCK_ROW_3_COL_5_MAIN ENDP
	
    CHECK_COUNTER_SCORE_ROW_3_COL_5 PROC NEAR
        mov cx, COUNTER_SCORE_ROW_3_COL_5
        cmp cx, 0h
        je ADD_SCORE_ROW_3_COL_5
        ret
        ADD_SCORE_ROW_3_COL_5:
        mov ax, SCORE
        add ax, 1h
        mov SCORE, ax
        ret
    CHECK_COUNTER_SCORE_ROW_3_COL_5 ENDP
	
	DRAW_BLOCK_ROW_3_COL_5 PROC NEAR
        mov cx, BLOCK_X_COL_5
        mov dx, BLOCK_Y_ROW_3
        DRAW_BLOCK_3_5_HORIZONTAL:
            mov ah, 0Ch 
            mov al, 02h ; zelena boja
            mov bh, 00h;
            int 10h
            
            inc cx 
            mov ax, cx
            sub ax, BLOCK_X_COL_5
            cmp ax, BLOCK_SIZE_X
            jng DRAW_BLOCK_3_5_HORIZONTAL
            
            mov cx, BLOCK_X_COL_5;
            inc dx
            
            mov ax, dx
            sub ax, BLOCK_Y_ROW_3
            cmp ax, BLOCK_SIZE_Y
            jng DRAW_BLOCK_3_5_HORIZONTAL
        ret
	DRAW_BLOCK_ROW_3_COL_5 ENDP
	
	
    DRAW_PLATFORM PROC NEAR
        mov cx,PLATFORM_X ; postavi inicijalnu kolonu (X)
		mov dx,PLATFORM_Y ; postavi inicijalni red (Y)
		
		DRAW_PLATF:
			mov ah,0Ch ; podesi konfiguraciju za ispis piksela
			mov al,01h ; izaberi plavu boju
			mov bh,00h ; 
			int 10h    ; izvrsi konfiguraciju
			
			inc cx     ;cx = cx + 1
			mov ax,cx  
			sub ax,PLATFORM_X ;cx - PLATFORM_X > PLATFORM_WIDTH (ako jeste, iscrtali smo za taj red sve kolone; inace nastavljamo dalje)
			cmp ax,PLATFORM_WIDTH
			jng DRAW_PLATF
			
			mov cx, PLATFORM_X   ; vrati cx na inicijalnu kolonu
			inc dx               ; idemo u sledeci red
			
			mov ax,dx    ; dx - PLATFORM_Y > PLATFORM_HEIGHT (ako jeste, iscrtali smo sve redove piksela; inace nastavljamo dalje)
			sub ax,PLATFORM_Y
			cmp ax,PLATFORM_HEIGHT
			jng DRAW_PLATF
		
		ret
	DRAW_PLATFORM ENDP
	
	CLEAR_SCREEN PROC NEAR
			mov ah,00h ; postaviti konfiguraciju za video mod
			mov al,13h ;
			int 10h    ; izvrsi konfiguraciju
		
			mov ah,0bh ; postavi konfiguraciju  za boju pozadine
			mov bh,00h ;
			mov bl,00h ; boja pozadine = crna
			int 10h    ; izvrsi konfiguraciju
			
			ret
	CLEAR_SCREEN ENDP

		
KRAJ_IGRE:
        mov ah,00h ; postaviti konfiguraciju za video mod
        mov al,13h ;
        int 10h    ; izvrsi konfiguraciju
		
        mov ah,0bh ; postavi konfiguraciju  za boju pozadine
        mov bh,00h ;
        mov bl,00h ; boja pozadine = crna
        int 10h    ; izvrsi konfiguraciju
        
        mov ah, 9h          ;konfiguracija za ispis stringa
        lea dx, GAME_OVER   ;ucitaj string za kraj igre
        int 21h             ;izvrsi konfiguraciju
        
        mov ah, 0           ;konfiguracija za slusanje sa tastature
        int 16h             ;izvrsi konfiguraciju
        cmp ah, 19h         ;da li je pritisnuto 'p'
        je PLAY_AGAIN       ;ako jeste skoci na labelu za setovanje ponovnog igranja
        cmp ah, 12h         ;da li je pritisnuto 'e'
        je EXIT             ;ako jeste skoci na izlaz koji ce da vrati konzolu
        ret

CONGRATULATIONS:
        mov ah,00h ; postaviti konfiguraciju za video mod
        mov al,13h ;
        int 10h    ; izvrsi konfiguraciju
		
        mov ah,0bh ; postavi konfiguraciju  za boju pozadine
        mov bh,00h ;
        mov bl,00h ; boja pozadine = crna
        int 10h    ; izvrsi konfiguraciju
        
        mov ah, 9h
        lea dx, CONGRATULATIONS_STRING
        int 21h

        mov ah, 0 
        int 16h
        cmp ah, 19h
        je PLAY_AGAIN
        cmp ah, 12h
        je EXIT
        ret
    
EXIT:
        call CLEAR_SCREEN   ;setovanje crne pozadine
        jmp kraj            ;skoci na kraj u kome se vraca konzola korisniku
        
PLAY_AGAIN:
        mov TIME_AUX, 0h    ;ponovno setovanje vremenske promjenjive
            
        mov BALL_X, 9Eh     ;vracanje pocetne x koordinate lopte
        mov BALL_Y, 0BAh    ;vracanje pocetne y koordinate lopte
        mov BALL_VELOCITY_X, 05h    ;vracanje pocetne horizontalne komponente brzine
        mov BALL_VELOCITY_Y, -04h   ;vracanje pocetne vertikalne komponente brzine
        
        mov COUNTER_BLOCK_ROW_1_COL_1, 1h   ;vracanje pocetne vrijednosti brojaca za provjeru da li je udaren blok
        mov COUNTER_SCORE_ROW_1_COL_1, 0h   ;vracanje pocetne vrijednosti brojaca za provjeru da li je blok udaran prvi put (tj. da li dodati poene)
        
        mov COUNTER_BLOCK_ROW_1_COL_2, 1h
        mov COUNTER_SCORE_ROW_1_COL_2, 0h
        
        mov COUNTER_BLOCK_ROW_1_COL_3, 1h
        mov COUNTER_SCORE_ROW_1_COL_3, 0h
        
        mov COUNTER_BLOCK_ROW_1_COL_4, 1h
        mov COUNTER_SCORE_ROW_1_COL_4, 0h
        
        mov COUNTER_BLOCK_ROW_1_COL_5, 1h
        mov COUNTER_SCORE_ROW_1_COL_5, 0h
        
        mov COUNTER_BLOCK_ROW_2_COL_1, 1h
        mov COUNTER_SCORE_ROW_2_COL_1, 0h
        
        mov COUNTER_BLOCK_ROW_2_COL_2, 1h
        mov COUNTER_SCORE_ROW_2_COL_2, 0h
        
        mov COUNTER_BLOCK_ROW_2_COL_3, 1h
        mov COUNTER_SCORE_ROW_2_COL_3, 0h
        
        mov COUNTER_BLOCK_ROW_2_COL_4, 1h
        mov COUNTER_SCORE_ROW_2_COL_4, 0h
        
        mov COUNTER_BLOCK_ROW_2_COL_5, 1h
        mov COUNTER_SCORE_ROW_2_COL_5, 0h
        
        mov COUNTER_BLOCK_ROW_3_COL_1, 1h
        mov COUNTER_SCORE_ROW_3_COL_1, 0h
        
        mov COUNTER_BLOCK_ROW_3_COL_2, 1h
        mov COUNTER_SCORE_ROW_3_COL_2, 0h
        
        mov COUNTER_BLOCK_ROW_3_COL_3, 1h
        mov COUNTER_SCORE_ROW_3_COL_3, 0h
        
        mov COUNTER_BLOCK_ROW_3_COL_4, 1h
        mov COUNTER_SCORE_ROW_3_COL_4, 0h
        
        mov COUNTER_BLOCK_ROW_3_COL_5, 1h
        mov COUNTER_SCORE_ROW_3_COL_5, 0h

        mov PLATFORM_X, 6Eh     ;vracanje pocetne x koordinate platforme
        mov PLATFORM_Y, 0BEh    ;vracanje pocetne y koordinate platforme
        
        mov COLOR, 0Fh      ;vracanje koda za bijelu boju u promjenjivu COLOR
        mov SCORE, 0h       ;vracanje vrijednosti poena na 0
    
        call CHECK_TIME     ;pozivanje glavne procedure za pokretanje igre

kraj:   mov ax, 4c00h		 ; exit  (vraca konzolu)
		int 21h

cseg 	ends
; ---- Zavrsen code segment


; ---- Stek segment
sseg segment stack 'STACK' 
     dw 64 dup(?)
sseg ends
; ---- Zavrsen stek segment

; ---- Definisanje ulazne tacke programa (video)
end draw
