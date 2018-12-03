from math import pow, log
 
def convert(nbr, old_base, new_base):
    " nbr est un str, old_base et new_base des entiers "
    chiffres = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    nbr_dec = 0
    i = len(nbr) - 1
    for x in nbr:
        nbr_dec += chiffres.index(x) * int(pow(old_base, i))
        i -= 1
    nbr_chars = int(log(nbr_dec)/log(new_base) + 1)
    nbr_final = ""
    for i in range(0, nbr_chars):
        x = int(nbr_dec / pow(new_base, nbr_chars-1-i))
        nbr_dec -= int(x * pow(new_base, nbr_chars-1-i))
        nbr_final += chiffres[x]
    return nbr_final
 
print (convert("1000", 10, 6))
