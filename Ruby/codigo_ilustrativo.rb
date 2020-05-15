# Variable Local:
## Son visibles dentro del bloque que las inicializo.
age = 10
_Age = 20

# Variable Global:
## Son visibles desde cualquier bloque.
$age = 10

# Variables de Instancia:
## Todos los objetos de una clase cuentan con la variable,
## pero el valor es privado para cada uno de ellos.
@age = 20 

# Variables de Clase:
## Todos los objetos de una clase comparten esa variable
## (incluido el valor).
@@age = 20





a = 10;  
b = 15; 
  
# if statement
if a % 2 == 0  
     puts "Even Number" 
end
  
# if-else statement
if b % 2 == 0  
    puts "Even Number" 
else
    puts "Odd Number" 
end 

# Even Number
# Odd Number


# La funcion each permite aplicar un bloque de instrucciones
# a todos los elementos de la misma. En este fragmento podemos
# ver como llama a los elementos 'n' y los escribe.
["Esto", "son", "numeros"].each do |n|     
    puts n     
end




# Bucle for basico
for a in 1..5 do  
    puts "Salu2"  
end

# Bucle while basico
i = 0
while i < 10
    puts "Hey You"
    i = i + 1
end

# Se puede hacer uso de breaks para controlar los bucles:
# Bucle do-while basico construido con breaks.
loop do
    val = '7'
        
    # using boolean expressions  
    if val == '7'
        break
    end
end 







# Super clase Salu2
class Salu2   
    
    def initialize   

        puts "Esto es la superclase"
    end
          
    def super_method  
            
        puts "Metodo de la superclase"
    end
end
    
# Clase Hola, subclase de Salu2  
class Hola < Salu2   
    
    def initialize   
    
       puts "Esto es una subclase"
    end
end
    
# Creacion de un objeto de superclase
Salu2.new
    
# Creacion de un objeto de subclase
sub_obj = Hola.new
    
# Mediante herencia, podemos llamar a metodos de
# la superclase desde la subclase
sub_obj.super_method  


# Un modulo contiene variables, metodos, constantes y 
# variables de clase. Se definen del mismo modo que una clase

# Creacion del modulo LP
# Utilizamos el nombre del modulo com prefijo 
module LP  
        
    Nota = 10;  
    
    def LP.welcome  
        puts "Welcome to LP!!"
    end
          
    def LP.tutorial    
        puts "Leete el manual"
    end
          
    def LP.truth    
        puts "Haskell > C++"
    end
        
end
     
# displaying the value of   
# module constant  
puts LP::Nota
    
# calling the methods of the module  
LP.welcome  
LP.tutorial  
LP.truth  



# Los arrays pueden contener elementos de distintos tipos.

myArray = ["Como", "Me", "Gusta", "Ruby"]

myArray[1]  ## Retorna "Como"
myArray[-1] ## Retorna "Ruby" (los indices son ciclicos)



