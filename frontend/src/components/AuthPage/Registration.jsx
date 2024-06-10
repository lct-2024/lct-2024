import React, { useState } from 'react'
import style from "./Registration.module.css"
import Navigation from '../Navigation'
import Footer from '../Footer'
import { Link, useNavigate } from 'react-router-dom'
import axios from 'axios'

const Registration = () => {

    const [formData, setFormData] = useState({
        email: '',
        fio: '',
        metadata: {},
    });
    const [errorMessage, setErrorMessage] = useState(null);
    const navigate = useNavigate();

    const handleChange = (event) => {
        setFormData({ ...formData, [event.target.name]: event.target.value });
    };

    const handleSubmit = async (event) => {
        event.preventDefault();

        try {
            const response = await axios.post('https://passport.lct24.dev.40ants.com/api/signup', {
                jsonrpc: '2.0',
                method: 'signup',
                params: {
                    email: formData.email,
                    fio: formData.fio,
                    metadata: formData.metadata,
                },
                id: 1
            });


            if (response.ok) {
                const data = await response.json();
                console.log('Успешная регистрация:', data);
                navigate('/login');
            } else {
                const errorData = await response.json();
                setErrorMessage(errorData.message || 'Ошибка регистрации');
            }
        } catch (error) {
            console.error('Ошибка при отправке данных:', error);
            setErrorMessage('Произошла ошибка при регистрации');
        }
    };
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