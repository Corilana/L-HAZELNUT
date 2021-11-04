# Comments and questions

 * In Dataframes/DATASET_multiplebuds.xlsx: sequences in 2020 and 2021 do not seem to contain the same kind of information. How can we build a model that would represent successions of annual shoots in 2020 and 2021?
 * It seems that metamers composing annual shoots are encoded as successive lines with the same values of "shoot1yo". It appears that "length" and "length1yo" are constant for a given "shoot1yo", so the length and length1yo should be the total length of the annual shoot composed by these metamers and we have no information of each metamer length?
 * Is there any polycyclism to handle as in MappleT (reproductive GU potentially followed by a sylleptic vegetative GU?)
 *FG: (4/11/2021) I trained with N-APPLE_seq_hsmc.py code. I understood the logic behind but I still have some questions regarding some lines of the code. I put them into a Power point presentation that can  may be shared in the next meeting