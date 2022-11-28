import subprocess
import re

main_cmd = 'prolog -g runTest({nb_iterations},{ia1},{ia2}),halt -f ../testIAs.pl'

nb_iterations = 10

#préparer analyse via matplotlib
'''lines={}
lines[0]=' ;'
for i in range(2,9):
	lines[0] = lines[0]+str(i)+';'
	lines[i-1] = str(i)+';'''

lines = []

#[IA1],[IA2],[IA_Debute],[NbParties],[Tps IA Gagnante],[KMoy IA Gagnante]
k=0
with open('resultat_test.csv','w') as f:
	for i in range(2,19):
		for j in range(i,19):
			if i == j:
				continue
			if i == 2 and j in [5, 6, 7, 8, 10, 11, 14, 15, 16, 19]:
				continue
			if j == 16 and i not in [15, 11, 8, 7]:
				continue
			
			cmd = main_cmd.format(nb_iterations=nb_iterations,ia1=i,ia2=j)
			cmd_list = cmd.split()
			retour = subprocess.check_output(cmd_list).decode("utf-8")

			s = retour.split("\n")
			
			stats_ia1_regex = re.search('^Si IA1 commence : KMoy\(IA1\)=?([\d.]+(?:e-?\d+)?)? KMoy\(IA2\)=?([\d.]+(?:e-?\d+)?)? Tps\(IA\)=?([\d.]+(?:e-?\d+)?)?s Tps\(IA\)=?([\d.]+(?:e-?\d+)?)?s.*$', s[0])
			stats_ia2_regex = re.search('^Si IA2 commence : KMoy\(IA1\)=?([\d.]+(?:e-?\d+)?)? KMoy\(IA2\)=?([\d.]+(?:e-?\d+)?)? Tps\(IA\)=?([\d.]+(?:e-?\d+)?)?s Tps\(IA\)=?([\d.]+(?:e-?\d+)?)?s.*$', s[1])

			ia1_regex = re.search('^(.+) en commençant : a gagné (\d+) fois et a perdu (\d+).*$', s[len(s)-2])
			ia2_regex = re.search('^(.+) en commençant : a gagné (\d+) fois et a perdu (\d+).*$', s[len(s)-1])
			
			ia1_commencant_kmoy = float(stats_ia1_regex.group(1))
			ia1_commencent_ia2_kmoy = float(stats_ia1_regex.group(2))
			ia1_commencant_tps = float(stats_ia1_regex.group(3))
			ia1_commencant_tps_ia2 = float(stats_ia1_regex.group(4))

			nom_ia1 = ia1_regex.group(1)
			nom_ia2 = ia2_regex.group(1)

			ia1_gagne_commencant = float(ia1_regex.group(2))
			ia1_perd_commencant = float(ia1_regex.group(3))
			
			ia2_commencant_kmoy = float(stats_ia2_regex.group(1))
			ia2_commencent_ia1_kmoy = float(stats_ia2_regex.group(2))
			ia2_commencant_tps = float(stats_ia2_regex.group(3))
			ia2_commencant_tps_ia1 = float(stats_ia2_regex.group(4))

			ia2_gagne_commencant = float(ia2_regex.group(2))
			ia2_perd_commencant = float(ia2_regex.group(3))

			#[IA1],[IA2],[nbVictoiresIA1],[nbDefaitesIA1],[IA_Debute],[Moy Tps IA Gagnante],[KMoy IA Gagnante]
			lines.append(nom_ia1 + "," + nom_ia2 + "," + str(ia1_gagne_commencant) + "," + str(ia1_perd_commencant) + "," + str(ia1_commencant_tps_ia2) + "," + str(ia1_commencant_tps) + "," + str(ia1_commencant_kmoy+ia1_commencent_ia2_kmoy))
			
			lines.append(nom_ia2 + "," + nom_ia1 + "," + str(ia2_gagne_commencant) + "," + str(ia2_perd_commencant) + "," + str(ia2_commencant_tps_ia1) + "," + str(ia2_commencant_tps) + "," + str(ia2_commencant_kmoy+ia2_commencent_ia1_kmoy))

			print("(",i,",",j,")")

			f.write(lines[k])
			f.write('\n')
			k += 1
			'''if i-1 not in lines:
				lines[i-1] = ''
			lines[i-1] = lines[i-1] + str((ia1_gagne_commencant-ia1_perd_commencant)/nb_iterations) + ';'
			if(i!=j):
				if j-1 not in lines:
					lines[j-1] = ''
				lines[j-1] = lines[j-1] + str((ia2_gagne_commencant-ia2_perd_commencant)/nb_iterations) + ';'''

	print(lines)


