import React from 'react'
import style from "./Registration.module.css"
import Navigation from '../Navigation'
import Footer from '../Footer'
import { Link } from 'react-router-dom'

const Registration = () => {
    return (
        <>
            <div className={style.main}>
                <div className='container'>
                    <div className={style.head}>
                        <Navigation />
                        <h1>РЕГИСТРАЦИЯ</h1>
                    </div>
                </div>
            </div>
            <div className='container'>
                <div className={style.body}>
                    <p>Чтобы оставлять резюме, комментировать и общаться с рекрутерами. <Link to="/login">Уже есть аккаунт, войти</Link></p>
                    <input type="text" placeholder='Имя' />
                    <input type="text" placeholder='Фамилия' />
                    <input type="text" placeholder='Отчество' />
                    <input type="email" placeholder='Почта' />
                    <input type="text" placeholder='Специальность' />
                    <button className={style.btn}>Войти</button>
                </div>
            </div>
            <Footer />
        </>
    )
}

export default Registration