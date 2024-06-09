import React, { useState } from 'react'
import style from './MainOpens.module.css'

const MainOpens = () => {

    const [comments, setComments] = useState([
        { name: "Иванов Иван Иванович", text: "Здравствуйте! Я не понимаю есть ли в офисе кошки, не нашел в описании компании." },
        { name: "Егоров Александр Петрович", text: "Здравствуйте! Можно ли совмещать работу с учебой?" }
    ])
    const [newCommentText, setNewCommentText] = useState('');
    const [selectedFilter, setSelectedFilter] = useState("Как устроиться на работу в Reksoft?");
    const [showInput, setShowInput] = useState(false)

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

    const handleShowInput = () => {
        setShowInput(true);
    };

    const handleCommentSubmit = () => {
        setComments([
            ...comments,
            { name: "Николай Семенович", text: newCommentText }
        ]);
        setNewCommentText('');
        setShowInput(false);
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
                <h2>Не нашли ответ на свой вопрос? Напишите в комментарии, чтобы получить ответ:</h2>
                <div className={style.comments}>
                    {comments.map((comment, i) => {
                        return <div className={style.comment} key={i}>
                            <div>
                                <h4>{comment.name}</h4>
                                <p>21.01.24  21.00</p>
                            </div>
                            <p>{comment.text}</p>
                        </div>
                    })}
                    {showInput && (<>
                        <textarea onChange={(e) => setNewCommentText(e.target.value)} value={newCommentText} placeholder='Комментарий...' />
                        <button className={style.btn} onClick={handleCommentSubmit}>
                            Отправить комментарий
                        </button>
                    </>)}
                    {showInput === false && <button className={style.btn} onClick={handleShowInput}>Написать комментарий</button>}
                </div>
                <h2>ОТКРЫТЫЕ ВАКАНСИИ</h2>
                <div className={style.vacansies}>
                    <p>здесь будут карточки вакансий, которые еще не готовы</p>
                    <button className={style.btn}>Смотреть все вакансии  <svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                        <path d="M5.00195 12H19.002M19.002 12L12.002 5M19.002 12L12.002 19" stroke="white" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                    </svg></button>
                </div>
            </div>
        </div >
    )
}

export default MainOpens