import React from 'react'
import style from './Footer.module.css'
import Navigation from './Navigation'

const Footer = ({ showText }) => {

    return (
        <div className={style.main}>
            <div className="container">
                <div className={style.footer}>
                    {showText && <h4>Познакомься с командой напрямую</h4>}
                    {showText && <p>В VC, Habr, VK и Telegram мы делимся мероприятиями, внутренней жизнью и опытом Рексофт</p>}
                    <Navigation />
                </div>
            </div>
        </div>
    )
}

export default Footer