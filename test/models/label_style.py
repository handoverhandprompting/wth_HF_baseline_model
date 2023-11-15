import streamlit as st

def styling():
    # number input's label control
    st.markdown(
		"""<style>
		div[class*="stNumberInput"] > label > div[data-testid="stMarkdownContainer"] > p {
			font-size: 18px;
            font-weight:bold
		}
		</style>
		""", unsafe_allow_html=True)

    # Selectbox label control
    st.markdown(
		"""<style>
		div[class*="row-widget stSelectbox"] > label > div[data-testid="stMarkdownContainer"] > p {
			font-size: 18px;
            font-weight:bold;
		}
		</style>
		""", unsafe_allow_html=True)
    

    # Checkbox of "None" const width
    st.markdown("""
    <style>
        [data-baseweb="checkbox"]{
        width: 100px;
        }
    </style>
    """, unsafe_allow_html=True)


    st.markdown("""<style>
                *[@id="root"] > div > div{
                overflow-wrap: anywhere;
                }
                </style>""", unsafe_allow_html=True)