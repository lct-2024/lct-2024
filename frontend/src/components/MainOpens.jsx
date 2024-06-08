import React from 'react'
import style from './MainOpens.module.css'

const MainOpens = () => {
    return (
        <div className='container'>
            <div className={style.main}>
                <h2>ОТВЕТЫ НА ВОПРОСЫ</h2>
                <div className={style.mainText}>
                    <div className={style.smallBlock}>
                        <div>
                            <p>Интересные задачи и передовые технологии</p>
                        </div>
                        <div>
                            <p>Команда профессионалов</p>
                        </div>
                        <div>
                            <p>Самореализация в крупной стабильной компании</p>
                        </div>
                        <div>
                            <p>Оформление и полис ДМС</p>
                        </div>
                    </div>
                    <div className={style.text}>
                        <p>Для этого вам нужно успешно пройти несколько шагов:
                            Найти подходящую вашему опыту и интересам вакансию;
                            Откликнуться на интересующую вакансию и прикрепить резюме;
                            Если ваше резюме нам подходит — пройти интервью со специалистом по подбору персонала и руководителем направления, в котором открыта эта вакансия;
                            Получить официальное предложение о работе.
                        </p>
                    </div>
                </div>
                <h2>Не нашли ответ на свой вопрос? Напишите в комментарии, чтобы получить ответ:</h2>
                <div className={style.comments}>
                    <div className={style.comment}>
                        <div>
                            <h4>Иванов Иван Иванович</h4>
                            <p>21.01.24  21.00</p>
                        </div>
                        <p>Здравствуйте! Я не понимаю есть ли в офисе кошки, не нашел в описании компании.</p>
                    </div>
                    <div className={style.comment}>
                        <div>
                            <h4>Егоров Александр Петрович</h4>
                            <p>21.01.24  21.00</p>
                        </div>
                        <p>Здравствуйте! Можно ли совмещать работу с учебой?</p>
                    </div>
                    <button className={style.btn}>Написать комментарий</button>
                </div>
                <h2>ОТКРЫТЫЕ ВАКАНСИИ</h2>
                <div className={style.vacansies}>
                    <p>здесь будут карточки вакансий, которые еще не готовы</p>
                    <button className={style.btn}>Смотреть все вакансии  <svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                        <path d="M5.00195 12H19.002M19.002 12L12.002 5M19.002 12L12.002 19" stroke="white" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                    </svg></button>
                </div>
            </div>
        </div>
    )
}

export default MainOpens