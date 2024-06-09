import React, { useState } from 'react'
import style from './VacansyList.module.css'
import { Link } from 'react-router-dom'

const VacansyList = ({ vacansies }) => {
    return (
        <div className='container'>
            <div className={style.list}>
                {vacansies.map((vacansy, i) => {
                    return <div className={style.vacansy} key={i}>
                        <h2>{vacansy.title}</h2>
                        <p>{vacansy.salary}</p>
                        <p>{vacansy.date}</p>
                        <div className={style.body}>
                            <div className={style.filter}>
                                <p>Разработка</p>
                                <p>Москва</p>
                                <p>Сортировочные центры “Почты России”</p>
                            </div>
                            <p>Совпадение с резюме {vacansy.coincidence}%</p>
                        </div>
                    </div>
                })}
            </div>
            <button className={style.btn}>Загрузить ещё</button>
        </div >
    )
}

export default VacansyList