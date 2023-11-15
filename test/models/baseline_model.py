import streamlit as st
from streamlit_card import card
from PIL import Image
from time import sleep
from .label_style import styling




# ========= RISK CALCULATION ========== 
def risk_calculation(dialysis, bun, age, lvef_2d_none, lvef_2d, esd_none, esd, rdw_cv_none, rdw_cv, ivsd_none, ivsd, bmi, lvmi_none, lvmi,
		     		nt_proBNP_none, nt_proBNP, paod, total_acei, p2y12, ar_none, ar_value, en_h_display, nyha, rvdd_none, rvdd, ua_u_0, alt_none,
					alt, lad_none, lad):
	# 設定基礎值 = 1
	base = 1

	# Dialysis & BUN 相關計算
	if not dialysis:
		base = base * 2.587773475155
		if bun:
			base = base * 1.02726707220
	# Age & LVEF 相關計算
	if not lvef_2d_none and 30.855 < age <= 53.454 and lvef_2d > 29.865:
		base = base * 0.370899348669

	# ESD 相關計算
	if not esd_none and esd > 0:
		base = base * 1.308434995962 

	# RDW_CV.yes.x.RDW_CV.b14.283_GAM.M 相關計算
	if not rdw_cv_none and rdw_cv > 14.283:
		base  = base * 1.673903885394
	
	# IVSd.yes.x.IVSd 相關計算
	if not ivsd_none and ivsd > 0:
		base = base * 0.304176246171
	
	# BMI 相關計算
	if bmi <= 25.999:
		base = base  * 1.602682458991
	
	# LVMI.yes.x.LVMI.se134.942.b199.801_PSpline.M 相關計算
	if not lvmi_none and (lvmi <= 134.943 or lvmi > 199.801):
		base = base * 1.632691237159
	
	# NT_proBNP.yes 相關計算
	if not nt_proBNP_none:
		if nt_proBNP > 2481.283:
			base = base * 2.988649424659
		else:
			base = base * 0.309312902691
	
	# PAOD 相關計算
	if paod:
		base = base * 2.563594874514
	
	# BaselineDrug_Yes.x.GDMT_RAASB_equi_0.se140.135_PSpline.S 相關計算
	if total_acei <= 140.135:
		base = base * 2.117629069429
	
	# BaselineDrug_Yes.x.P2Y12_U_0 相關計算
	if p2y12:
		base = base * 0.530893848359
	
	# AR.b0 相關計算
	if not ar_none and ar_value > 0:
		base = base * 0.634792500514

	# En_H 相關計算
	if en_h_display == 'Inpatient Department (IPD)':
		base = base * 0.554342784288

	# NYHA.12 相關計算
	if nyha == 1 or nyha == 2:
		base = base * 0.619875121344
	
	# RVDd.yes.x.RVDd.b2.469_PSpline.S 相關計算
	if not rvdd_none and rvdd > 2.469:
		base = base * 1.576956583161

	# BaselineDrug_Yes.x.UA_U_0 相關計算
	if ua_u_0:
		base = base * 0.532995989302
	
	# ALT.yes.x.ALT.se15.614.b84.255_PSpline.S 相關計算
	if not alt_none and (alt <= 15.614 or alt > 84.255):
		base = base * 1.494730577548

	# LAD.yes.x.LAD.b4.348_PSpline.M 相關計算
	if not lad_none and lad > 4.348:
		base = base * 1.658467832301
	
	# 回傳值
	return base


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

	card_value = str(card_value)

	hasClicked = card('Estimated Hazard Ratio', text=card_value, styles={"card":{
		"width": "500px",
		"height": "300px",
		"border-radius": "60px",
		"overflow-wrap": "anywhere"
	},
	"text": {
            "font-size": "36px"}})


	st.write('---')
	
	st.write(' ')

	with st.container():
		st.subheader('Baseline status')
		age = st.text_input('Age')
		if age:
			try:
				age = float(age)
			except:
				st.warning('請輸入數字')
		bmi = 0
		col1, col2, = st.columns(2)
		col1, col1_1, col2, col2_2 = st.columns([5,2,5,2])
		with col1_1:
			height_unit=st.selectbox("Height_unit",["cm","in"])
		with col2_2:
			weight_unit=st.selectbox("Weight_unit",["kg","lbs"])
		with col1:
			height = st.text_input('Height ') 
			if height:
				try:
					height = float(height)
					if height_unit=="in":
						height=height*2.54
				except:
					st.warning('請輸入數字')
		with col2:
			weight = st.text_input('Weight ') 
			if weight:
					try:
						weight = float(weight)
						if weight_unit=="lbs":
							weight=weight*0.45359237
					except:
						st.warning('請輸入數字')
		if height and weight:
			bmi =round( weight/((height/100) ** 2),2)
            
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
										 ['Class II (Slight limitation of physical activity. Comfortable at rest. Ordinary physical activity results in fatigue, palpitation, shortness of breath or chest pain.)', 2], ['Class III (Marked limitation of physical activity. Comfortable at rest. Less than ordinary activity causes fatigue, palpitation, shortness of breath or chest pain.)', 3],
										 ['Class IIII (Symptoms of heart failure at rest. Any physical activity causes further discomfort.)', 4]], format_func=lambda x: x[0], help='New York Heart Association functional classification')
			nyha = nyha_display[1]
			paod_diplay = st.selectbox('PAOD', [['yes', 1], ['no', 0]], format_func= lambda x: x[0], help='Peripheral Arterial Occlusive Disease')
			paod = paod_diplay[1]
		with col2:
			dialysis_display = st.selectbox('Dialysis', [['yes', 1], ['no', 0]], format_func= lambda x: x[0])
			dialysis = dialysis_display[1]
		
	st.write('---')
	with st.container():
		st.subheader('Drug use')
		col1, col2 = st.columns([5, 2])
		with col1:
				acei_display = st.selectbox('ACEI/ARB', ['None', 'valsartan', 'losartan', 'captopril', 'enalapril', 'ramipril', 'Not mentioned above'], help='Angiotensin Converting Enzyme Inhibitors/Angiotensin Receptor Blockers')
		with col2:
			acei_dose = st.text_input('Dose(mg)', disabled=(acei_display == 'None' or acei_display == 'Not mentioned above'))
			if acei_dose:
				try:
					acei_dose = float(acei_dose)
				except:
					st.warning('請輸入數字')

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
			en_h_display = st.selectbox('Initiation time of Entresto(sacubitril/valsartan)', ['Outpatient Department (OPD)', 'Inpatient Department (IPD)', 'None']) # How to put into model?
		with col2:
			ua_u_o_display = st.selectbox('Urate-lowering Agents', ['None', 'allopurinol', 'benzbromarone', 'febuxostat', 'probenecid', 'rasburicase', 'sulfinpyrazone', 'Not mentioned above'])
			if ua_u_o_display == 'None' or ua_u_o_display == 'Not mentioned above':
				ua_u_0 = 0
			else:
				ua_u_0 = 1
		col1, col2 = st.columns(2)
		with col1:
			p2y12_display =st.selectbox('P2Y12 Receptor Inhibitors', ['None', 'clopidogrel', 'prasugrel', 'ticagrelor', 'Not mentioned above'])
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
				bun = st.text_input('(mg/dL)', disabled=bun_none, help='Blood Urea Nitrogen')
				if bun:
					try:
						bun = float(bun)
					except:
						st.warning('請輸入數字')
			with col2:
				st.write(f'###### NT_proBNP ######')
				nt_proBNP_none = st.checkbox('None', key='nt_proBNP')
			with col3:
				nt_proBNP = st.text_input('(pg/mL)', disabled=nt_proBNP_none, help='N-Terminal Pro-Brain (or B-type) Natriuretic Peptide')
				if nt_proBNP:
					try:
						nt_proBNP = float(nt_proBNP)
					except:
						st.warning('請輸入數字')

		st.write(' ')

		with st.container():
			col1, col1_1, space1, col2, col2_1 = st.columns([3, 6, 3, 3, 6])	
			with col1:
				st.write(f'###### ALT ######')
				alt_none = st.checkbox('None', key='alt')
			with col1_1:
				alt = st.text_input('(U/L)', disabled=alt_none, key='alt_', help='Alanine Aminotransferase')
				if alt:
					try:
						alt = float(alt)
					except:
						st.warning('請輸入數字')
			with col2:
				st.write(f'###### RDW_CV ######')
				rdw_cv_none = st.checkbox('None', key='rdw_cv')
			with col2_1:
				rdw_cv = st.text_input('(%)', disabled=rdw_cv_none, key='rdw_cv_', help='Red Cell Distribution Width_Coefficient of Variation')
				if rdw_cv:
					try:
						rdw_cv = float(rdw_cv)
					except:
						st.warning('請輸入數字')
	
	st.write('---')

	with st.container():
		st.subheader('Cardiac parameters of echocardiography')
		col1, col1_1, space1, col2, col2_1, space2, col3, col3_1 = st.columns([1, 2, 1, 1, 2, 1, 1, 2])	
		with col1:
			st.write(f'###### AR ######')
			ar_none = st.checkbox('None', key='ar_none')
		with col1_1:
			ar = st.selectbox(' ', [['trace/trivial', 0.5], ['mild', 1], ['mild to moderate', 1.5], ['moderate', 2], ['moderate to severe', 2.5], ['severe', 3]], label_visibility='visible', disabled=ar_none, help='Aortic Regurgitation',  format_func= lambda x: x[0])
			ar_value = ar[1]
		with col2:
			st.write(f'###### RV ######')
			rvdd_none = st.checkbox('None', key='rvdd')
		with col2_1:	
			rvdd = st.text_input('(cm)', key='rvdd_', disabled=rvdd_none, help='RVDd, Right Ventricular Diastolic Dimension')
			if rvdd:
					try:
						rvdd = float(rvdd)
					except:
						st.warning('請輸入數字')
		with col3:
			st.write(f'###### IVSd ######')
			ivsd_none = st.checkbox('None', key='ivsd')	
		with col3_1:
			ivsd = st.text_input('(cm)',  key='ivsd_', disabled=ivsd_none, help='Interventricular Septum Dimension')
			if ivsd:
					try:
						ivsd = float(ivsd)
					except:
						st.warning('請輸入數字')
	with st.container():
		col1, col1_1, space1, col2, col2_1, space2, col3, col3_1 = st.columns([1, 2, 1, 1, 2, 1, 1, 2])
		with col1:
			st.write(f'###### ESD ######')
			esd_none = st.checkbox('None', key='esd')
		with col1_1:
			esd = st.text_input('(cm)', key='esd_', disabled=esd_none, help='End Systolic Dimension = LVIDs, Left Ventricular Internal Diameter End Systole')
			if esd:
					try:
						esd = float(esd)
					except:
						st.warning('請輸入數字')
		with col2:
			st.write(f'###### LAD ######')
			lad_none = st.checkbox('None', key='lad')
		with col2_1:
			lad = st.text_input('(cm)', key='lad_', disabled=lad_none, label_visibility='visible', help='LAD, Left Atrial Diameter')	
			if lad:
					try:
						lad = float(lad)
					except:
						st.warning('請輸入數字')
		with col3:
			st.write(f'###### LVMI ######')
			lvmi_none = st.checkbox('None', key='lvmi')
		with col3_1:
			lvmi = st.text_input('(g/m2)', key='lvmi_', disabled=lvmi_none, label_visibility='visible', help='Left Ventricular Mass Index')
			if lvmi:
					try:
						lvmi = float(lvmi)
					except:
						st.warning('請輸入數字')
	with st.container():
		col1, col1_1, space1, col2, col2_1, space2, col3, col3_1 = st.columns([1, 2, 1, 1, 2, 1, 1, 2])
		with col1:
			st.write(f'###### LVEF_2D ######')
			lvef_2d_none = st.checkbox('None', key='lvef_2d')
		with col1_1:
			lvef_2d = st.text_input('(%)', key='lvef_2d_', disabled=lvef_2d_none, label_visibility='visible', help='Left Ventricular Ejection Fraction_2D = EF MOD-sp4, Ejection Fraction Method of Disks-Single Plane, Apical 4 Chamber')
			if lvef_2d:
					try:
						lvef_2d = float(lvef_2d)
					except:
						st.warning('請輸入數字')
		

	st.write('---')
	if st.button('Enter'):
		try:
			risk_value = risk_calculation(dialysis=dialysis, bun=bun, age=age, lvef_2d_none=lvef_2d_none, lvef_2d=lvef_2d, esd_none=esd_none, esd=esd, rdw_cv_none=rdw_cv_none, rdw_cv=rdw_cv, ivsd_none=ivsd_none, ivsd=ivsd, bmi=bmi, lvmi_none=lvmi_none, lvmi=lvmi,
						nt_proBNP_none=nt_proBNP_none, nt_proBNP=nt_proBNP, paod=paod, total_acei=total_acei, p2y12=p2y12, ar_none=ar_none, ar_value=ar_value, en_h_display=en_h_display, nyha=nyha, rvdd_none=rvdd_none, rvdd=rvdd, ua_u_0=ua_u_0, alt_none=alt_none,
						alt=alt, lad_none=lad_none, lad=lad)

			st.session_state['risk_value'] = risk_value
			sleep(0.3)
			st.experimental_rerun()
		except:
			st.error('輸入有誤，請檢查欄位')


