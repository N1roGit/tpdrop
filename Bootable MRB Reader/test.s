.code16                   #generate 16-bit code
.data
.text                    
     .globl _start
     .globl print
     .globl unpack
     .globl print_s
_start:
        Win:.ascii "Win"
        Lin:.ascii "Lin"
        SyS:.ascii "Sys"
        p2:.ascii "\r\n"
        NoS:.ascii "\r\nNoS"
        LBA:.ascii "\r\nLBA"
        CHS_E: .ascii "\r\nCSH_E"
        Type: .ascii "\r\nType"
        CHS_I: .ascii "\r\nCSH_I"
        Boot: .ascii  "\r\nBOOT"
        N_Part: .space 1 

    movb $0x00 , %ah
    movb $0x80 , %dl
    int $0x13
    read:
    movb $0x02 , %ah  # Sub-operacion
    movb $0x01 , %al  # Numero de sectores a leer
    movb $0x00 , %ch  # Numero de cilindro
    movb $0x01 , %cl  # Numero de sector
    movb $0x00 , %dh  # Numero de cabeza
    movb $0x81 , %dl  # Unidad a leer

    movw $0x800,%bx
    movw %bx , %es
    movw $0x00 , %bx # guarda lo que leyo en
                     # [es:bx] osea 0x8000
    int $0x13
    jc FIN

    movb $4 , %dh
    movw $0x81BE,%di
    movb $0 , %ah
    
    while:
            movb $16,%cl # Llevo cuenta de los bytes que no mire
            movw $0,%si # Aca llevo los que ya mire
            addw $16,%di
        check:
            cmpb $0 , %cl
            je No_Particion # 16 bytes de 0
            decb %cl
            incw %si
            cmpb $0 , (%di)
            jne Particion
            decw %di 
            jmp check

        Particion:
            incb %ah     # aca llevo la cantidad de particiones del disco
            addw %si,%di # vuelvo para atras todo que lei de los 16 
            decb %dh     #  
            cmpb $0,%dh  # Me fijo si ya lei las 4 particiones
            jnz while
        No_Particion:   # No tengo mas particiones una
                        # vez que encuentro los primeros 16 0s
                        # Aca ya tengo la cantidad de particiones No-Nulas
            cmpb $0 , %ah
            jz FIN
        # Si no hay particiones salta al final
    movb %ah , N_Part


    movb %ah ,%al
    movb $16 ,%cl
    mul %cl
    # al = 16*Num_part =  16~64
    movw $0x81BE , %di
    xorw %bx , %bx  
    Load_stack:
        movb (%di) , %bl 
        push %bx
        incw %di 
        decb %al
        cmpb $0 ,%al
        jne Load_stack
    # Cargo byte a byte las tablas el stack, y arreglo la endianess 
    movb N_Part, %ch
    Printing:        movw $p2, %di
        movw $2, %si
        call print_s
        movb %ch , %al
        call print
        # Imprime Numero de particion
        movw $NoS , %di
        movw $5,%si
        call print_s
        movb $8 , %cl
        Number_of_Sectors_LBA:
            pop %dx
            call unpack
            # Parte alta
            movb %dh ,%al
            call print
            # Parte baja
            movb %dl ,%al
            call print
            decb %cl
            cmpb $4 , %cl
            jne a1
            movw $LBA , %di
            movw $5 , %si
            call print_s
            # aca imprimo la cadena LBA o algo para marcar que estoy leyendo el lba ahora
            a1:
            cmpb $0 , %cl
            jne Number_of_Sectors_LBA

        movw $CHS_E , %di
        movw $7,%si
        call print_s
        chse:
            pop %dx
            call unpack
            pop %di # me guardo tambien el s
            movw %di , %ax
            shrb $6 , %al # entonces me quedo con 00c9c8 y el S queda a salvo en di
            # Parte mas alta
            call print
            # Parte alta
            movb %dh, %al
            call print
            # Parte Baja
            movb %dl ,%al
            call print
            movb $141,%al # separador 
            call print
            movw %di,%dx
            call unpack
            # Parte alta
            movb %dh, %al
            andb $3,%al # borro los ultimos 2 bits (de los 4 que tiene) 0000 0011
            call print
            # Parte Baja
            movb %dl ,%al
            call print
            movb $141,%al # separador 
            call print
            pop %dx
            call unpack
            # Parte alta
            movb %dh, %al
            call print
            # Parte Baja
            movb %dl ,%al
            call print
            cmpb $254,%bl
            je isboot
        type:
            movw $Type ,%di
            movw $6 , %si
            call print_s
            pop %dx

            movw $3,%si
            cmpw $0x00 ,%dx
            je Numero
            cmpw $0x05, %dx
            je EXTENDED
            call unpack
            cmpb $0x0 ,%dh
            je windows
            cmpb $0x8 ,%dh
            je linux
            # aca imprimo O de otro
            jmp otro
            subtipo:
            cmpb $0x3 ,%dl
            je sy
            cmpb $0xf ,%dl
            je sy
            cmpb $0x7 ,%dl
            je sy
            Numero:
                # Parte alta
                movb %dh, %al
                call print
                # Parte Baja
                movb %dl ,%al
                call print
        
        # truco para no copiar y pegar 2 veces el codigo del csh
        movw $CHS_I , %di
        movw $7,%si
        call print_s
        movb $254 , %bl
        jmp chse
        isboot:
        # boot:
            xorb %bl ,%bl
            pop %dx
            cmp $0x80 , %dl
            jne boot
            movw $Boot ,%di
            movw $6 ,%si
            call print_s
 
        boot:
        decb %ch
        cmpb $0 ,%ch
        jne Printing
    FIN:
        movb $0x00, %ah
        int $0x16
        cmpb $13 ,%al # 13 == enter
        je read
    jmp FIN
unpack:
    movb %dl,%dh # copio el num
    andb $15,%dl # Aago la mitad de arriba
    shrb $4 ,%dh # Apago la mitad de abajo
ret
windows:
    movw $Win,%di
    call print_s
    jmp subtipo
linux:
    movw $Lin , %di
    call print_s
    jmp subtipo
EXTENDED:
    movb $0x0e ,%al
    call print
    movb $0x00 , %al
    int $0x10
    int $0x10
    jmp Numero
sy:
    movw $SyS , %di
    movw $3, %si
    call print_s
    jmp Numero
otro:
    movb 0x18,%al
    call print
    jmp Numero
print:
    addb $48,%al
    cmpb $58, %al
    jnae f1 
    addb $7,%al # si es hexa
    f1:
    movb $0x0e , %ah 
    int $0x10
ret
# en di llevo la dir del string , y en si la cant de chars
print_s:
    l:
    movb $0x0e , %ah
    movb (%di) ,%al
    decw %si
    incw %di
    int $0x10
    cmpw $0, %si
    jne l
    movb $0x00 , %al
    int $0x10
    # int $0x10
ret

     . = _start + 510     #mov to 510th byte from 0 pos
     .word 0xaa55         #firma de booteo
