import React, { useState, useEffect } from 'react';
import style from "./VacansyPage.module.css";
import Navigation from '../Navigation';
import Footer from '../Footer';
import VacansyList from './VacansyList';
import { useSelector, useDispatch } from 'react-redux';
import { fetchVacansies, setVacansies } from '../../store/vacansiesSlise';
import axios from 'axios';
import Comments from '../Comments';

const VacansyPage = () => {
    const API_BASE_URL = 'https://chat.lct24.dev.40ants.com/api'
    const vacansies = useSelector(state => state.vacansies.data)
    const dispatch = useDispatch()
    const authToken = useSelector(state => state.auth.token);
    const [originalVacansies, setOriginalVacansies] = useState([]);
    const [searchTerm, setSearchTerm] = useState('')

    useEffect(() => {
        dispatch(fetchVacansies());
    }, [dispatch]);

    useEffect(() => {
        if (vacansies.length > 0) {
            setOriginalVacansies(vacansies);
        }
    }, []);

    const handleSearchChange = (e) => {
        const searchTerm = e.target.value;
        setSearchTerm(searchTerm);
        handleSearchSubmit(searchTerm);
    };

    const handleSearchSubmit = (searchTerm) => {
        if (searchTerm.trim() === '') {
            console.log(originalVacansies)
            dispatch(setVacansies(originalVacansies));
        } else {
            const filteredVacancies = originalVacansies.filter(vacansy =>
                vacansy.title.toLowerCase().includes(searchTerm.toLowerCase())
            );
            dispatch(setVacansies(filteredVacancies));
        }
    };
    const handleKeyPress = (event) => {
        if (event.key === 'Enter') {
            handleSearchSubmit(searchTerm);
        }
    };

    return (
        <section>
            <div className={style.main}>
                <div className='container'>
                    <div className={style.head}>
                        <Navigation />
                        <h1>ВАКАНСИИ</h1>
                    </div>
                </div>
            </div>
            <div className={style.body}>
                <div className="container">
                    <div className={style.search}>
                        <input
                            type="text"
                            placeholder='Поиск...'
                            value={searchTerm}
                            onChange={handleSearchChange}
                            onKeyPress={handleKeyPress}
                        />
                        <div>Все категории
                            <svg width="24" height="19" viewBox="0 0 24 19" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M6 7L12 13L18 7" stroke="black" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" />
                            </svg>
                        </div>
                        <div>Все специальности
                            <svg width="24" height="19" viewBox="0 0 24 19" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M6 7L12 13L18 7" stroke="black" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" />
                            </svg>
                        </div>
                        <div>Все города
                            <svg width="24" height="19" viewBox="0 0 24 19" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M6 7L12 13L18 7" stroke="black" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" />
                            </svg>
                        </div>
                        <div>Все проекты
                            <svg width="24" height="19" viewBox="0 0 24 19" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M6 7L12 13L18 7" stroke="black" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" />
                            </svg>
                        </div>
                    </div>
                </div>
                {vacansies.length === 0 ? <h1 style={{ margin: "0 auto" }}>Загрузка...</h1> : <VacansyList hideBody='false' vacansies={vacansies} />}
                <Comments text="вакансиях" />
                <Footer />
            </div>
        </section>
    );
}

export default VacansyPage;
