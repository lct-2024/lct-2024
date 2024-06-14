import React, { useState } from 'react'
import style from './MainOpens.module.css'
import VacansyList from './VacansyPage/VacansyList';
import { useSelector } from 'react-redux';
import Comments from './Comments';

const MainOpens = () => {
    const vacansies = useSelector(state => state.vacansies)
    const [selectedFilter, setSelectedFilter] = useState("Как устроиться на работу в Reksoft?");

    const handleFilterClick = (filter) => {
        setSelectedFilter(filter === selectedFilter ? null : filter);
    };

    const getFilterText = () => {
        switch (selectedFilter) {
            case 'Как устроиться на работу в Reksoft?':
                return `Для этого вам нужно успешно пройти несколько шагов:
    Найти подходящую вашему опыту и интересам вакансию;
    Откликнуться на интересующую вакансию и прикрепить резюме;
    Если ваше резюме нам подходит — пройти интервью со специалистом по подбору персонала и руководителем направления, в котором открыта эта вакансия;
    Получить официальное предложение о работе.`;
            case 'Есть ли помощь иногородним при переезде?':
                return `Да, Reksoft помогает иногородним гражданам при переезде, в случае положительной оценки резюме рекрутером.`;
            case 'Я откликнулся на вакансию. Какие дальнейшие действия?':
                return `Отлично, теперь вам надо подождать ответ, после того, как ваше резюме проверят. Обычно проверка занимает 3 рабочих дня.`;
            case 'Можно ли устроиться на стажировку в «Reksoft»?':
                return `Конечно! «Рексофт» проводит трехмесячные стажировки для выпускников технических вузов и молодых специалистов. Привлечение молодых специалистов – это постоянная линия в развитии нашей команды`;
            default:
                return '';
        }
    };


    return (
        <div className='container'>
            <div className={style.main}>
                <h2>ОТВЕТЫ НА ВОПРОСЫ</h2>
                <div className={style.mainText}>
                    <div className={style.smallBlock}>
                        <div className={selectedFilter === 'Как устроиться на работу в Reksoft?' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Как устроиться на работу в Reksoft?')}>
                            <p>Как устроиться на работу в Reksoft?</p>
                        </div>
                        <div className={selectedFilter === 'Есть ли помощь иногородним при переезде?' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Есть ли помощь иногородним при переезде?')}>
                            <p>Есть ли помощь иногородним при переезде?</p>
                        </div>
                        <div className={selectedFilter === 'Я откликнулся на вакансию. Какие дальнейшие действия?' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Я откликнулся на вакансию. Какие дальнейшие действия?')}>
                            <p>Я откликнулся на вакансию. Какие дальнейшие действия?</p>
                        </div>
                        <div className={selectedFilter === 'Можно ли устроиться на стажировку в «Reksoft»?' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Можно ли устроиться на стажировку в «Reksoft»?')}>
                            <p>Можно ли устроиться на стажировку в «Reksoft»?</p>
                        </div>
                    </div>
                    <div className={style.text}>
                        <p>{getFilterText()}</p>
                    </div>
                </div>
                <h2>ОТКРЫТЫЕ ВАКАНСИИ</h2>
                {vacansies.length === 0 ? <h1 style={{ margin: "0 auto" }}>Загрузка...</h1> : <VacansyList hideBody={false} btnShow="false" />}
                <Comments text="нас" />
            </div>
        </div >
    )
}

export default MainOpens