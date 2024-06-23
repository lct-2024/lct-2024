import React, { useState } from 'react';
import style from "./Registration.module.css";
import Navigation from '../Navigation';
import Footer from '../Footer';
import { Link, useNavigate } from 'react-router-dom';
import axios from 'axios';

const Registration = () => {
    const [formData, setFormData] = useState({
        email: '',
        password: '',
        fio: '',
        specialty: '',
    });
    const [errorMessage, setErrorMessage] = useState(null);
    const navigate = useNavigate();

    const handleChange = (event) => {
        const { name, value } = event.target;
        setFormData({ ...formData, [name]: value });
    };

    const handleSubmit = async (event) => {
        event.preventDefault();

        try {
            const response = await axios.post('https://passport.lct24.dev.40ants.com/api/signup', {
                jsonrpc: '2.0',
                method: 'signup',
                params: {
                    email: formData.email,
                    password: formData.password,
                    fio: formData.fio,
                    metadata: {
                        specialty: formData.specialty
                    },
                },
                id: 1
            });

            if (response.data.error) {
                setErrorMessage(response.data.error.message || 'Ошибка регистрации');
            } else {
                console.log('Успешная регистрация:', response.data.result);
                navigate('/login');
            }
        } catch (error) {
            console.error('Ошибка при отправке данных:', error);
            setErrorMessage('Произошла ошибка при регистрации');
        }
    };

    return (
        <div className={style.wrap}>
            <div className={style.main}>
                <div className='container'>
                    <div className={style.head}>
                        <Navigation />
                        <h1>РЕГИСТРАЦИЯ</h1>
                    </div>
                </div>
            </div>
            <div className='container'>
                <form onSubmit={handleSubmit} className={style.body}>
                    <p>Чтобы оставлять резюме, комментировать и общаться с рекрутерами. <Link to="/login">Уже есть аккаунт, войти</Link></p>
                    <input
                        type="text"
                        placeholder='ФИО'
                        name="fio"
                        value={formData.fio}
                        onChange={handleChange}
                        required
                    />
                    <input
                        type="email"
                        placeholder='Почта'
                        name="email"
                        value={formData.email}
                        onChange={handleChange}
                        required
                    />
                    <input
                        type="password"
                        placeholder='Пароль'
                        name="password"
                        value={formData.password}
                        onChange={handleChange}
                        required
                    />
                    <input
                        type="text"
                        placeholder='Специальность'
                        name="specialty"
                        value={formData.specialty}
                        onChange={handleChange}
                    />
                    <button type="submit" className={style.btn}>Зарегистрироваться</button>
                </form>
                {errorMessage && <p className={style.error}>{errorMessage}</p>}
            </div>
            <Footer />
        </div>
    );
}

export default Registration;
