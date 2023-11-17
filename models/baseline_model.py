import streamlit as st
from streamlit_card import card
from math import exp
from time import sleep
# from .label_style import styling
import base64


# ========= RISK CALCULATION ==========
def risk_calculation(dialysis, bun, age, lvef_2d_none, lvef_2d, esd_none, esd, rdw_cv_none, rdw_cv, ivsd_none, ivsd, bmi, lvmi_none, lvmi, nt_proBNP_none, nt_proBNP, paod, total_acei, p2y12, ar_none, ar_value, en_h_display, nyha, rvdd_none, rvdd, ua_u_0, alt_none, alt, lad_none, lad):
	# 設定基礎值 = 1
	base = 1

	# Exponential 部分數值
	# Dialysis & BUN 相關計算
	if dialysis == 0:
		sub1 = 0.026028463109 * bun
	else:
		sub1 = 0
	print('sub1: ', sub1)

	# Dialysis 相關計算
	if dialysis:
		sub2 = 0.950797843888
	else:
		sub2 = 0
	print('sub2: ', sub2)

	# Age & LVEF 相關計算
	if 30.855 < age <= 53.454 and lvef_2d_none is False and lvef_2d > 29.865:
		sub3 = -0.991824550572
	else:
		sub3 = 0
	print('sub3: ', sub3)

	# ESD 相關計算
	if esd_none is False and esd:
		sub4 = 0.268831763467 * esd
	else:
		sub4 = 0
	print('sub4: ', sub4)

	# RDW_CV.yes.x.RDW_CV.b14.283_GAM.M 相關計算
	if rdw_cv_none is False and rdw_cv > 14.283:
		sub5 = 0.515158554286
	else:
		sub5 = 0
	print('sub5: ', sub5)

	# IVSd.yes.x.IVSd 相關計算
	if ivsd_none is False and ivsd:
		sub6 = -1.19014798842 * ivsd
	else:
		sub6 = 0
	print('sub6: ', sub6)

	# BMI 相關計算
	if bmi < 25.999:
		sub7 = 0.471678762296
	else:
		sub7 = 0
	print('sub7: ', sub7)

	# LVMI.yes.x.LVMI.se134.942.b199.801_PSpline.M 相關計算
	if lvmi_none is False and (lvmi < 134942 or lvmi > 199.801):
		sub8 = 0.490229719050
	else:
		sub8 = 0
	print('sub8: ', sub8)

	# NT_proBNP.yes 相關計算
	if nt_proBNP_none is False and nt_proBNP > 2481.283:
		sub9 = 1.094821587918
	elif nt_proBNP_none is False and nt_proBNP <= 2481.283:
		sub9 = -1.173401884380
	else:
		sub9 = 0
	print('sub9: ', sub9)

	# PAOD 相關計算
	if paod:
		sub10 = 0.941410521315
	else:
		sub10 = 0
	print('sub10: ', sub10)

	# BaselineDrug_Yes.x.GDMT_RAASB_equi_0.se140.135_PSpline.S 相關計算
	if total_acei <= 140.135:
		sub11 = 0.750297099372
	else:
		sub11 = 0
	print('sub11: ', sub11)

	# BaselineDrug_Yes.x.P2Y12_U_0 相關計算
	if p2y12:
		sub12 = -0.633193186654
	else:
		sub12 = 0
	print('sub12: ', sub12)

	# AR.b0 相關計算
	if ar_none is False and ar_value > 0:
		sub13 = -0.454457104335
	else:
		sub13 = 0
	print('sub13: ', sub13)

	# En_H 相關計算
	if en_h_display == 'Inpatient Department (IPD)':
		sub14 = -0.589972039375
	else:
		sub14 = 0
	print('sub14: ', sub14)

	# NYHA.12 相關計算
	if nyha == 1 or nyha == 2:
		sub15 = -0.478237238417
	else:
		sub15 = 0
	print('sub15: ', sub15)

	# RVDd.yes.x.RVDd.b2.469_PSpline.S 相關計算
	if rvdd_none is False and rvdd > 23469:
		sub16 = 0.455496776315
	else:
		sub16 = 0
	print('sub16: ', sub16)

	# BaselineDrug_Yes.ua_u_0 相關計算
	if ua_u_0 == 1:
		sub17 = -0.629241379606
	else:
		sub17 = 0
	print('sub17: ', sub17)

	# ALT.yes.x.ALT.se15.614.b84.255_PSpline.S 相關計算
	if alt_none is False and (alt < 15.614 or alt >= 84.255):
		sub18 = 0.401945974915
	else:
		sub18 = 0
	print('sub18: ', sub18)

	# LAD.yes.x.LAD.b4.348_PSpline.M 相關計算
	if lad_none is False and lad > 4.348:
		sub19 = 0.505894183559
	else:
		sub19 = 0
	print('sub19: ', sub19)

	# 列印個數值
	print('-' * 100)
	value_list = [sub1, sub2, sub3, sub4, sub5, sub6, sub7, sub8, sub9, sub10, sub11, sub12, sub13, sub14, sub15, sub16, sub17, sub18, sub19]

	text = ''
	for value in value_list:
		text += (str(value) + ' + ')
	print(text)
	print('-' * 100)
	print('Total exponential: ', sum(value_list))

	# 回傳值
	return base * exp(sum(value_list))


# ========= VIEW ===========
def baseline_view():
	# Styling

	st.title('Baseline Model')
	# init session
	if 'risk_value' not in st.session_state:
		st.session_state['risk_value'] = 0
		card_value = st.session_state['risk_value']
	else:
		card_value = st.session_state['risk_value']

	card_value = str(round(card_value, 3))

	def inactivate():
		pass

	# Card background figure(which is essential)
	with open('./bbk.png', "rb") as f:
		data = f.read()
		encoded = base64.b64encode(data)
	data = "data:image/png;base64," + encoded.decode("utf-8")

	# Give card a size by st column and container
	space_1, location, space_2 = st.columns([1, 2, 1])

	with location:
		hasClicked = card('Estimated Hazard Ratio', image=data, text=card_value, styles={"card": {
			"width": "auto",
			"height": "300px",
			"border-radius": "60px",
			"overflow-wrap": "anywhere"
		},
			"text": {
				"font-size": "2.5em",
				"overflow-wrap": "anywhere"}}, on_click=inactivate)

	st.write('---')

	st.write(' ')

	# Hight, weight and BMI
	with st.container():
		st.subheader('Baseline status')
		age = st.number_input('Age', step=1)
		bmi = 0
		col1, col1_1, col2, col2_2 = st.columns([5, 2, 5, 2])
		with col1_1:
			height_unit = st.selectbox("Height_unit", ["cm", "in"])
		with col2_2:
			weight_unit = st.selectbox("Weight_unit", ["kg", "lbs"])
		with col1:
			height = st.number_input('Height ', step=1)
			if height:
				if height_unit == "in":
					height = height * 2.54
		with col2:
			weight = st.number_input('Weight ', step=1)
			if weight:
				if weight_unit == "lbs":
					weight = weight * 0.45359237
		if height and weight:
			bmi = round(weight / ((height / 100) ** 2), 2)

		col1, col2, col3 = st.columns([1, 3, 4])
		with col1:
			st.markdown('##### ' + 'BMI: ' + ' #####')
			with col2:
				if bmi is not None:
					st.markdown('##### ' + str(bmi) + ' #####')

	st.write('---')

	with st.container():
		st.subheader('Disease status')
		col1, col2 = st.columns(2)
		with col1:
			nyha_display = st.selectbox('NYHA', [['None/Unclassified', 0],
												 ['Class I (No limitation of physical activity. Ordinary physical activity does not cause undue fatigue, palpitation or shortness of breath.)', 1],
												 ['Class II (Slight limitation of physical activity. Comfortable at rest. Ordinary physical activity results in fatigue, palpitation, shortness of breath or chest pain.)', 2],
												 ['Class III (Marked limitation of physical activity. Comfortable at rest. Less than ordinary activity causes fatigue, palpitation, shortness of breath or chest pain.)',  3],
												 ['Class IV (Symptoms of heart failure at rest. Any physical activity causes further discomfort.)', 4]], format_func=lambda x: x[0],
										help='New York Heart Association functional classification')
			nyha = nyha_display[1]
			paod_diplay = st.selectbox('PAOD', [['yes', 1], ['no', 0]], format_func=lambda x: x[0], help='Peripheral Arterial Occlusive Disease')
			paod = paod_diplay[1]
		with col2:
			dialysis_display = st.selectbox('Dialysis', [['yes', 1], ['no', 0]], format_func=lambda x: x[0])
			dialysis = dialysis_display[1]

	st.write('---')
	with st.container():
		st.subheader('Drug use')
		col1, col2 = st.columns([5, 2])
		with col1:
			acei_display = st.selectbox('ACEI/ARB',
										['None', 'valsartan', 'losartan', 'captopril', 'enalapril', 'ramipril', 'Not mentioned above'],
										help='Angiotensin Converting Enzyme Inhibitors/Angiotensin Receptor Blockers')
		with col2:
			acei_dose = st.number_input('Dose(mg)', disabled=(acei_display == 'None' or acei_display == 'Not mentioned above'))
			if acei_dose:
				if acei_display == 'Valsartan':
					total_acei = acei_dose
				elif acei_display == 'Losartan' or acei_display == 'Captopril':
					total_acei = acei_dose * 32 / 15
				elif acei_display == 'Enalapril':
					total_acei = acei_dose * 16
				elif acei_display == 'Ramipril':
					total_acei = acei_dose * 32
				else:
					total_acei = 0
			else:
				total_acei = 0

		col1, col2 = st.columns(2)
		with col1:
			en_h_display = st.selectbox('Initiation time of Entresto(sacubitril/valsartan)',
										['Outpatient Department (OPD)', 'Inpatient Department (IPD)', 'None'])
		with col2:
			ua_u_o_display = st.selectbox('Urate-lowering Agents', ['None', 'allopurinol', 'benzbromarone', 'febuxostat', 'probenecid', 'rasburicase', 'sulfinpyrazone', 'Not mentioned above'])
			if ua_u_o_display == 'None' or ua_u_o_display == 'Not mentioned above':
				ua_u_0 = 0
			else:
				ua_u_0 = 1
		col1, col2 = st.columns(2)
		with col1:
			p2y12_display = st.selectbox('P2Y12 Receptor Inhibitors', ['None', 'clopidogrel', 'prasugrel', 'ticagrelor', 'Not mentioned above'])
			if p2y12_display == 'None' or p2y12_display == 'Not mentioned above':
				p2y12 = 0
			else:
				p2y12 = 1

	st.write('---')

	with st.container():
		st.subheader('Lab data')
		with st.container():
			col1, col1_1, space, col2, col3 = st.columns([3, 6, 3, 3, 6])
			with col1:
				st.write(f'###### BUN ######')
				bun_none = st.checkbox('None', key='bun_none')
			with col1_1:
				bun = st.number_input('(mg/dL)', disabled=bun_none, help='Blood Urea Nitrogen')

			with col2:
				st.write(f'###### NT_proBNP ######')
				nt_proBNP_none = st.checkbox('None', key='nt_proBNP')
			with col3:
				nt_proBNP = st.number_input('(pg/mL)', disabled=nt_proBNP_none, help='N-Terminal Pro-Brain (or B-type) Natriuretic Peptide')

		st.write(' ')

		with st.container():
			col1, col1_1, space1, col2, col2_1 = st.columns([3, 6, 3, 3, 6])
			with col1:
				st.write(f'###### ALT ######')
				alt_none = st.checkbox('None', key='alt')
			with col1_1:
				alt = st.number_input('(U/L)', disabled=alt_none, key='alt_', help='Alanine Aminotransferase')

			with col2:
				st.write(f'###### RDW_CV ######')
				rdw_cv_none = st.checkbox('None', key='rdw_cv')
			with col2_1:
				rdw_cv = st.number_input('(%)', disabled=rdw_cv_none, key='rdw_cv_', help='Red Cell Distribution Width_Coefficient of Variation')

	st.write('---')

	with st.container():
		st.subheader('Cardiac parameters of echocardiography')
		col1, col1_1, space1, col2, col2_1 = st.columns([3, 6, 3, 3, 6])
		with col1:
			st.write(f'###### AR ######')
			ar_none = st.checkbox('None', key='ar_none')
		with col1_1:
			ar = st.selectbox(' ', [['trace/trivial', 0.5], ['mild', 1], ['mild to moderate', 1.5], ['moderate', 2],
									['moderate to severe', 2.5], ['severe', 3]], label_visibility='visible', disabled=ar_none, help='Aortic Regurgitation', format_func=lambda x: x[0])
			ar_value = ar[1]
		with col2:
			st.write(f'###### RV ######')
			rvdd_none = st.checkbox('None', key='rvdd')
		with col2_1:
			rvdd = st.number_input('(cm)', key='rvdd_', disabled=rvdd_none, help='RVDd, Right Ventricular Diastolic Dimension')

	with st.container():
		col1, col1_1, space1, col2, col2_1 = st.columns([3, 6, 3, 3, 6])
		with col1:
			st.write(f'###### IVSd ######')
			ivsd_none = st.checkbox('None', key='ivsd')
		with col1_1:
			ivsd = st.number_input('(cm)', key='ivsd_', disabled=ivsd_none, help='Interventricular Septum Dimension')
		
		with col2:
			st.write(f'###### LVMI ######')
			lvmi_none = st.checkbox('None', key='lvmi')
		with col2_1:
			lvmi = st.number_input('(g/m2)', key='lvmi_', disabled=lvmi_none, label_visibility='visible', help='Left Ventricular Mass Index')
		

	with st.container():
		col1, col1_1, space1, col2, col2_1 = st.columns([3, 6, 3, 3, 6])
		with col1:
			st.write(f'###### ESD ######')
			esd_none = st.checkbox('None', key='esd')
		with col1_1:
			esd = st.number_input('(cm)', key='esd_', disabled=esd_none, help='End Systolic Dimension = LVIDs, Left Ventricular Internal Diameter End Systole')

		with col2:
			st.write(f'###### LAD ######')
			lad_none = st.checkbox('None', key='lad')
		with col2_1:
			lad = st.number_input('(cm)', key='lad_', disabled=lad_none, label_visibility='visible', help='LAD, Left Atrial Diameter')
			

	with st.container():
		col1, col1_1, space1, col2, col2_1 = st.columns([3, 6, 3, 3, 6])
		with col1:
			st.write(f'###### LVEF_2D ######')
			lvef_2d_none = st.checkbox('None', key='lvef_2d')
		with col1_1:
			lvef_2d = st.number_input('(%)', key='lvef_2d_', disabled=lvef_2d_none, label_visibility='visible', help='Left Ventricular Ejection Fraction_2D = EF MOD-sp4, Ejection Fraction Method of Disks-Single Plane, Apical 4 Chamber')

	st.write('---')
	if st.button('Enter'):
		try:
			risk_value = risk_calculation(dialysis=dialysis, bun=bun, age=age, lvef_2d_none=lvef_2d_none,
										  lvef_2d=lvef_2d, esd_none=esd_none, esd=esd, rdw_cv_none=rdw_cv_none,
										  rdw_cv=rdw_cv, ivsd_none=ivsd_none, ivsd=ivsd, bmi=bmi, lvmi_none=lvmi_none,
										  lvmi=lvmi,
										  nt_proBNP_none=nt_proBNP_none, nt_proBNP=nt_proBNP, paod=paod,
										  total_acei=total_acei, p2y12=p2y12, ar_none=ar_none, ar_value=ar_value,
										  en_h_display=en_h_display, nyha=nyha, rvdd_none=rvdd_none, rvdd=rvdd,
										  ua_u_0=ua_u_0, alt_none=alt_none,
										  alt=alt, lad_none=lad_none, lad=lad)

			st.session_state['risk_value'] = risk_value
			sleep(0.2)
		except Exception as e:
			print(e)
			st.write(st.session_state['risk_value'])
			st.error('輸入有誤，請檢查欄位')
		finally:
			st.experimental_rerun()

# TODO: 數值計算有誤，用e (function exp()) 計算
# TODO: 取消文字輸入再檢查機制，用數字輸入模式(DONE)
# TODO: 依照PPT，有紅線區分1、0(可以用右側數值直接計算)，無紅線的用e
# TODO: Deploy上網
