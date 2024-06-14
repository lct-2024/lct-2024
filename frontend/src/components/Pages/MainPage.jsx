import React, { useState, useEffect } from 'react';
import style from './MainPage.module.css';
import Navigation from '../Navigation';
import OurProjects from '../OurProjects';
import Video from '../Video';
import ReksWork from '../ReksWork';
import MainOpens from '../MainOpens';
import Footer from '../Footer';
import { Link } from 'react-router-dom';
import { useDispatch } from 'react-redux';
import { fetchVacansies } from '../../store/vacansiesSlise';

const MainPage = () => {
    const texts = ["саморазвивайся", "работай в комфорте", "предлагай идеи"];
    const [currentTextIndex, setCurrentTextIndex] = useState(0);
    const dispatch = useDispatch();

    useEffect(() => {
        const intervalId = setInterval(() => {
            setCurrentTextIndex((prevIndex) => (prevIndex + 1) % texts.length);
        }, 2000);

        return () => clearInterval(intervalId);
    }, [texts.length]);

    useEffect(() => {
        dispatch(fetchVacansies());
    }, [dispatch]);

    return (
        <div className={style.wrapper}>
            <div className={style.main}>
                <div className="container">
                    <div className={style.sect}>
                        <Navigation />
                        <div className={style.hello}>
                            <h2>Присоединяйся к команде, <span>{texts[currentTextIndex]}</span></h2>
                            <div>
                                <div className={style.search}>
                                    <input type="text" placeholder='Поиск...' />
                                    <div>Все категории
                                        <svg width="24" height="19" viewBox="0 0 24 19" fill="none" xmlns="http://www.w3.org/2000/svg">
                                            <path d="M6 7L12 13L18 7" stroke="black" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" />
                                        </svg>
                                    </div>
                                    <div>Все специальности<svg width="24" height="19" viewBox="0 0 24 19" fill="none" xmlns="http://www.w3.org/2000/svg">
                                        <path d="M6 7L12 13L18 7" stroke="black" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" />
                                    </svg>
                                    </div>
                                    <div>Все города<svg width="24" height="19" viewBox="0 0 24 19" fill="none" xmlns="http://www.w3.org/2000/svg">
                                        <path d="M6 7L12 13L18 7" stroke="black" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" />
                                    </svg>
                                    </div>
                                    <button>
                                        <svg width="25" height="25" viewBox="0 0 25 25" fill="none" xmlns="http://www.w3.org/2000/svg">
                                            <path d="M21.5 21.5L17.2 17.2M19.5 11.5C19.5 15.9183 15.9183 19.5 11.5 19.5C7.08172 19.5 3.5 15.9183 3.5 11.5C3.5 7.08172 7.08172 3.5 11.5 3.5C15.9183 3.5 19.5 7.08172 19.5 11.5Z" stroke="white" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" />
                                        </svg>
                                    </button>
                                </div>
                                <div className={style.btns}>
                                    <button>Разработка</button>
                                    <button>Аналитика</button>
                                    <button>Менеджмент</button>
                                    <button>Тестирование</button>
                                    <button>Техподдержка</button>
                                </div>
                            </div>
                            <Link to="/vacansy-page" className={style.all}>Все вакансии<svg width="24" height="24" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M5.00195 12H19.002M19.002 12L12.002 5M19.002 12L12.002 19" stroke="white" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" />
                            </svg>
                            </Link>
                        </div>
                    </div>
                </div>
            </div>
            <OurProjects />
            <Video
                title="КОМФОРТНЫЙ ОФИС И РАБОЧЕЕ МЕСТО"
                text="Можно работать формате коворкинга, закрепленного рабочего места в кабинете или в open space. А еще мы позаботились об уютных кафетериях, удобных зонах отдыха и местах для развлечений."
                link="https://www.youtube.com/watch?v=fI4htZchBz0"
            />
            <ReksWork />
            <Video
                title="КЛАССНАЯ КОРПОРАТИВНАЯ КУЛЬТУРА"
                text="Политика открытых дверей:1) 2 раза в год проводим опрос удовлетворенности и улучшаем условия2) Не придерживаемся строгого дресс-кода3) Берем интервью у сотрудников, чтобы узнавать о них больше4) Награждаем в конце года лучших из лучших5) А еще мы любим и умеем хорошо отдыхать, поэтому у нас много праздников, как в онлайн, так и в офлайн формате"
                link="https://www.youtube.com/watch?v=9pZLCAIEUHM"
            />
            <MainOpens />
            <Footer showText="true" />
        </div>
    );
};

export default MainPage;