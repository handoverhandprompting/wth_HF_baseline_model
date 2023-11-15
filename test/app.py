import streamlit as st
from models import baseline_model, time_dependent_model

st.set_page_config(layout="wide")



def run():
    tab1, tab2 = st.tabs(['Baseline Model', 'Time-dependent Model'])
    with tab1:
        baseline_model.baseline_view()
    with tab2:
        time_dependent_model.time_dependent_view()



if __name__ == '__main__':
    run()