import React, { useState } from 'react'
import style from './VacansyList.module.css'

const VacansyList = () => {

    const [vacansies, setVacansies] = useState([
        { title: "Разработчик PLC", salary: "Зарплата по итогам собеседования", date: "8 июня 2024", coincidence: 80 },
        { title: "Технический архитектор/ Эксперт по технологическим вопросам 1С", salary: "от 120 000 до 250 000 ₽", date: "10 июня 2024", coincidence: 65 },
        { title: "Разработчик Node.JS", salary: "от 350 000 до 500 000 ₽", date: "18 июня 2024", coincidence: 65 },
        { title: "Ивент-менеджер", salary: "Зарплата по итогам собеседования", date: "12 июня 2024", coincidence: 10 }
    ])

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
        </div>
    )
}

export default VacansyList