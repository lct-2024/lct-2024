import React, { useState } from 'react'
import style from './VacansyList.module.css'
import { useNavigate } from 'react-router-dom';

const VacansyList = ({ vacansies }) => {
    const navigate = useNavigate();

    const handleVacancyClick = (vacansy) => {
        navigate(`/vacansy-info/${vacansy.id}`);
    };

    const [showCount, setShowCount] = useState(5);

    const handleLoadMore = () => {
        setShowCount(showCount + 5);
    };


    return (
        <div className='container'>
            <div className={style.list}>
                {vacansies.slice(0, showCount).map((vacansy, i) => {
                    return <div key={vacansy.id} className={style.vacansy}
                        onClick={() => handleVacancyClick(vacansy)}>
                        <h2>{vacansy.title}</h2>
                        <p>Зарплата: {vacansy.salary === null ? "Не указана" : vacansy.salary}</p>
                        <p>Дедлайн сбора откликов: {new Date(vacansy.active_to).toLocaleString()}</p>
                        <div className={style.body}>
                            <div className={style.filter}>
                                <p>{vacansy.type_of_employment}</p>
                                <p>{vacansy.city}</p>
                                <p>{vacansy.category}</p>
                            </div>
                            <p>Совпадение с резюме {vacansy.resume_matching_score}%</p>
                        </div>
                    </div>
                })}
            </div>
            <button onClick={handleLoadMore} className={style.btn}>Загрузить ещё</button>
        </div >
    )
}

export default VacansyList