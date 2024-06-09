import React from 'react'
import style from './VacansyInfo.module.css'
import Footer from '../Footer'
import Navigation from '../Navigation'

const VacansyInfo = () => {
    return (
        <div className={style.main}>
            <div className='container'>
                <div className={style.body}>
                    <Navigation />
                </div>
            </div>
            <Footer />
        </div>
    )
}

export default VacansyInfo