import React from 'react'
import style from "./Login.module.css"
import Footer from '../Footer'
import Navigation from '../Navigation'
import { Link } from 'react-router-dom'

const Login = () => {
    return (
        <>
            <div className={style.main}>
                <div className='container'>
                    <div className={style.head}>
                        <Navigation />
                        <h1>ВХОД</h1>
                    </div>
                </div>
            </div>
            <div className='container'>
                <div className={style.body}>
                    <p>Чтобы оставлять резюме, комментировать и общаться с рекрутерами. <Link to="/registration">Нет аккаунта, зарегистрироваться</Link></p>
                    <input type="email" placeholder='Почта' />
                    <button className={style.btn}>Войти</button>
                </div>
            </div>
            <Footer />
        </>
    )
}

export default Login