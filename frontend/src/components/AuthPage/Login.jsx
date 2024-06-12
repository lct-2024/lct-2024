import React, { useState } from 'react';
import style from "./Login.module.css";
import Footer from '../Footer';
import Navigation from '../Navigation';
import { Link, useNavigate } from 'react-router-dom';
import axios from 'axios';

const Login = ({ setAuthToken, setUser }) => {
    const [formData, setFormData] = useState({
        email: '',
        password: '',
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
            const response = await axios.post('https://passport.lct24.dev.40ants.com/api/login', {
                jsonrpc: '2.0',
                method: 'login',
                params: {
                    email: formData.email,
                    password: formData.password,
                },
                id: 1
            });

            if (response.data.error) {
                setErrorMessage(response.data.error.message || 'Ошибка входа');
            } else {
                const token = response.data.result;
                console.log('Успешный вход:', token);
                setUser(response.data.user);
                navigate('/profile');
                setAuthToken(token);
                localStorage.setItem('authToken', token);
            }
        } catch (error) {
            console.error('Ошибка при отправке данных:', error);
            setErrorMessage('Произошла ошибка при входе');
        }
    };

    return (
        <>
            <div className={style.main}>
                <div className='container'>
                    <div className={style.head}>
                        <Navigation />
                        <h1>ВХОД</h1>
                    </div>
                </div>
            </div>
            <div className='container'>
                <form onSubmit={handleSubmit} className={style.body}>
                    <p>Чтобы оставлять резюме, комментировать и общаться с рекрутерами. <Link to="/registration">Нет аккаунта, зарегистрироваться</Link></p>
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
                    <button type="submit" className={style.btn}>Войти</button>

                    {errorMessage && <p className={style.error}>{errorMessage}</p>}
                </form>
            </div>
            <Footer />
        </>
    );
};

export default Login;
