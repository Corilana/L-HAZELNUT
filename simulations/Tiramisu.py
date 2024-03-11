import kitchen
import power as pw
import random as rd
import kindness

people = 6
red_of_eggs = 4
sugar = 100
white_of_eggs = 4
mascarpone = 500
savoiardi = 300
cups_of_coffee = rd.randrange(4,8)
cups_of_water = rd.randrange(2,4)
cocoa_spoon = rd.randrange(5,10)
time_seconds = rd.randrange(4,8)
layer = rd.randrange(3,4)

cream_step1 = kitchen.put_in_a_pan(red_of_egs + sugar)
cream_step2 = pw.using_a_mixer(cream_step1)
cream_step3 = cream_step2 + mascarpone
cream_step4 = pw.using_a_mixer(cream_step3)
snow = pw.whip(white_of_eggs)
cream_step5 = patient.carefully_add(cream_step4,snow)
liquid = cups_of_coffee + cups_of_water
tiramisu_step1 = kitchen.put_in_liquid(savoiardi, time_seconds)
tiramisu_step2 = kitchen.put_in_a_pan(tiramisu_step1)

for l in layer:
    tiramisu_step3 = kindness.append(tiramisu_step2,cream_step5)
    tiramisu_step4 = kitchen.put_in_liquid(savoiardi, time_seconds)
    tiramisu_step5 = kindness.append(tiramisu_step3,tiramisu_step4)
    finish_tiramisu = kindness.append(tiramisu_step5,cocoa_spoon)
    print (finish_tiramisu)