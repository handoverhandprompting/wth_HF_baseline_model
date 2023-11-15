from setuptools import setup

def run():
    setup(
        name='test',
        version='0.0.1',
        packages=['streamlit', 'PIL', 'time']
    )


if __name__ =='__main__':
    run()