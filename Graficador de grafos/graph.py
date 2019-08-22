#! /usr/bin/python
# coding=utf-8

# 6ta Practica Laboratorio 
# Complementos Matematicos I
# Ejemplo parseo argumentos

# Para descargar py-gnuplot: http://sourceforge.net/projects/gnuplot-py/files/latest/download?source=files

import argparse
import Gnuplot
import random, math, time
from itertools import combinations 
from itertools import permutations

def leer_grafo_archivo(file_path):
    V = []
    E = []
    f = open(file_path, 'r')
    n = int(f.readline().strip(" \n\r"))
    for i in range(1, n+1):
        V.append(f.readline().strip(" \n\r"))
    for line in iter(f):
        [n0, n1] = line.strip(" \n\r").split(' ')
        E.append((n0,n1))
    return (V,E)

class LayoutGraph:
    
    def __init__(self, grafo, iters,temp,Width,Length,c,animate,verbose=False):

        # Guardo el grafo
        self.grafo = leer_grafo_archivo(grafo)
        self.aristas = self.grafo[1]
        self.nodos = self.grafo[0]
        self.posiciones = {}
        self.fuerzas = {}        
        
        # Guardo opciones
        self.iters = iters
        self.verbose = verbose
        self.animate = animate
        self.temp = temp
        self.length = Length
        self.width = Width
        self.c = c
        self.flag = 0
        self.k = (( (self.width*self.length) /len(self.nodos))**0.5)*self.c

    def randomize_positions(self):
        pos_x = {node:random.random()*self.width for node in self.nodos }
        pos_y = {node:random.random()*self.length for node in self.nodos }
        self.posiciones = (pos_x,pos_y)

    def init_acumulators(self):
        acum_x = {node:0 for node in self.nodos }
        acum_y = {node:0 for node in self.nodos }
        self.fuerzas = (acum_x,acum_y)

    def Fa(self,x):
        return (x**2)/self.k

    
    def Fr(self,x):
        return (self.k)/x
    
    def compute_repulsion_forces_redux(self):
        pos_x , pos_y = self.posiciones
        acum_x , acum_y = self.fuerzas
        for node in self.nodos:
            for node2 in self.nodos:
                if(node != node2):
                    delta_x = pos_x[node] - pos_x[node2]
                    delta_y = pos_y[node] - pos_y[node2]
                    modulo = (delta_x**2 + delta_y**2)**0.5
                    acum_x[node] += (delta_x/modulo) * self.Fr(modulo)
                    acum_y[node] += (delta_y/modulo) * self.Fr(modulo)
        self.fuerzas = (acum_x,acum_y)

    def compute_atraction_forces_redux(self):
        pos_x , pos_y = self.posiciones
        acum_x , acum_y = self.fuerzas
        for edge in self.aristas:
            node = edge[0]
            node2 = edge[1]
            delta_x = pos_x[node] - pos_x[node2]
            delta_y = pos_y[node] - pos_y[node2]
            modulo = ((delta_x**2 + delta_y**2)**0.5)
            acum_x[node] -= (delta_x/modulo) * self.Fa(modulo)
            acum_y[node] -= (delta_y/modulo) * self.Fa(modulo)
            acum_x[node2] += (delta_x/modulo) * self.Fa(modulo)
            acum_y[node2] += (delta_y/modulo) * self.Fa(modulo)
        self.fuerzas = (acum_x,acum_y)

    def update_positions_redux(self):
        pos_x , pos_y = self.posiciones
        acum_x , acum_y = self.fuerzas
        mov_x = {node:pos_x[node] for node in self.nodos}
        mov_y = {node:pos_y[node] for node in self.nodos}
        for node in self.nodos:

            modulo_fuerzas = (acum_x[node]**2 + acum_y[node]**2)**0.5
            pos_x[node] += (acum_x[node]/modulo_fuerzas) * min(modulo_fuerzas,self.temp)
            pos_y[node] += (acum_y[node]/modulo_fuerzas) * min(modulo_fuerzas,self.temp)

            pos_x[node] = min(self.width+random.random(), max(3+random.random(), pos_x[node]))
            pos_y[node] = min(self.length+random.random(), max(3+random.random(), pos_y[node]))
            if (self.verbose):
                print "Nodo : "+str(node) + " Fuerza acumulada :" +str(acum_x[node])+","+str(acum_y[node])
                print "Nodo : "+str(node) + " Posicion :" +str(pos_x[node])+","+str(pos_y[node])

            mov_x[node] = abs(mov_x[node] - pos_x[node])
            mov_y[node] = abs(mov_y[node] - pos_y[node])
            

        self.posiciones = (pos_x,pos_y)
        self.temp = self.temp*0.95
        aux = max( max( mov_x.values() ) , max( mov_y.values() ) )
	if (aux < 0.5 ):
            if (self.verbose):
                print("Movimiento despreciable de "+str(aux))
            self.flag = 1


    def compute_gravity_forces(self):
        pos_x , pos_y = self.posiciones
        acum_x , acum_y = self.fuerzas

        for n1 in self.nodos:

            dist = math.sqrt((pos_x[n1] - (self.width/2))**2 + (pos_y[n1] - (self.length/2))**2)
            if dist != 0:
                module_fa = self.Fa(dist)/10.0 #un orden de atraccion mas chico que las fuerzas de atraccion

                fx = module_fa*(pos_x[n1] - (self.width/2))/dist
                fy = module_fa*(pos_y[n1] - (self.length/2))/dist

                acum_x[n1] = acum_x[n1] - fx
                acum_y[n1] = acum_y[n1] - fy
                self.fuerzas = (acum_x,acum_y)

    def step(self):
        self.init_acumulators()
        self.compute_atraction_forces_redux()
        self.compute_repulsion_forces_redux()
        self.compute_gravity_forces()
        self.update_positions_redux()

    def init_pantalla(self):
        g = Gnuplot.Gnuplot()
        g('set xrange [0:'+ str(self.width)+ ']; set yrange [0:'+str(self.length)+']')
        g('unset key')
        g('plot NaN')
        return g

    
    def graficar(self,g):
        #Dibujo nodos
        x , y = self.posiciones
        for n in enumerate(self.nodos):
            node = n[1]
            number = n[0]+1 
            g('set object '+str(number)+' circle center '+ str(x[node])+','+str(y[node])+' size 3 ')
            #Dibujo aristas
        for arista in enumerate(self.aristas):
            u = arista[1][0]
            v = arista[1][1]
            numero = arista[0]+1 #numero las aristas para que no se rompa el gnuplot con letras
            g('set arrow '+str(numero)+' nohead from '+str(x[u])+','+str(y[u])+' to '+str(x[v])+','+str(y[v]))
        g('replot')    


    def layout(self):
        self.randomize_positions()
        g = self.init_pantalla()
        while self.temp > 0.1 :
            self.step()

            if (self.flag == 1):
                break

            if (self.iters < 0):
                if(self.verbose):
                    print "Se alcanzo el limite de las iteraciones"
                break
            if (self.animate != 0):
                self.graficar(g)
            time.sleep(self.animate)
            self.iters=self.iters-1
        self.graficar(g)
        raw_input("Presione enter para finalizar")


def main():
    # Definimos los argumentos de lina de comando que aceptamos
    parser = argparse.ArgumentParser()

    # Verbosidad, opcional, False por defecto
    parser.add_argument(
        '-v', '--verbose', 
        action='store_true', 
        help='Muestra mas informacion al correr el programa'
    )
    parser.add_argument(
        '-a', '--animate', 
        type=float, 
        help='Velocidad de los frames de la animacion con 0 salteandose la animacion',
        default = 0.0
    )
    # Cantidad de iteraciones, opcional, 50 por defecto
    parser.add_argument(
        '--iters',
        type=int,
        help='Cantidad de iteraciones a efectuar', 
        default=2222
    )
    # Temperatura inicial
    parser.add_argument(
        '--temp',
        type=float, 
        help='Temperatura inicial', 
        default=200.0
    )
    parser.add_argument(
        '--len',
        type=float, 
        help='Altura de la pantalla', 
        default=600.0
    )
    parser.add_argument(
        '--wid',
        type=float, 
        help='Anchura de la pantalla', 
        default=800.0
    )
    parser.add_argument(
        '--c',
        type=float, 
        help='Constante que escala el grafo', 
        default=5
    )
    # Archivo del cual leer el grafo
    parser.add_argument(
        'file_name',
        type = str,
        help='Archivo del cual leer el grafo a dibujar'
    )

    args = parser.parse_args()


    # Creamos nuestro objeto LayoutGraph
    layout_gr = LayoutGraph(
        args.file_name,  # TODO: Cambiar para usar grafo leido de archivo
        iters=args.iters,
        temp=args.temp,
        Width = args.wid,
        Length = args.len,
        c = args.c,
        animate=args.animate,
        verbose=args.verbose
        )
    
    # Ejecutamos el layout
    layout_gr.layout()
    return


if __name__ == '__main__':
    main()
