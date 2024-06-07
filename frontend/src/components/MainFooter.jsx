import React from 'react'
import style from './MainFooter.module.css'
import Navigation from './Navigation'

const MainFooter = () => {
    return (
        <div className={style.main}>
            <div className="container">
                <div className={style.footer}>
                    <h4>Познакомься с командой напрямую</h4>
                    <p>В VC, Habr, VK и Telegram мы делимся мероприятиями, внутренней жизнью и опытом Рексофт</p>
                    <Navigation />
                </div>
            </div>
        </div>
    )
}

export default MainFooter