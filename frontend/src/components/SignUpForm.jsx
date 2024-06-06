import React, { useState } from "react"
import axios from "axios"

const SignupForm = (onSignup) => {
    const [email, setEmail] = useState('')
    const [fio, setFio] = useState('')
    const [password, setPassword] = useState('')
    const [myRole, setMyRole] = useState('') // Добавлено поле myRole
    const [error, setError] = useState(null)

    const handleSubmit = async (e) => {
        e.preventDefault()
        setError(null)

        try {
            const response = await axios.post('https://passport.lct24.dev.40ants.com/api/signup', {
                jsonrpc: '2.0',
                method: 'signup',
                params: {
                    email,
                    fio,
                    password,
                    myRole
                },
                id: 0
            })

            console.log('Response:', response.data)
            const signupResult = response.data.signup_result



            if (signupResult === 'success') {
                if (response.data.token && typeof response.data.token === 'string') {
                    const token = response.data.token; // Получаем токен из ответа
                    localStorage.setItem("token", token);
                } else {
                    setError('Ошибка: Не удалось получить токен.');
                }
                onSignup();
            } else {
                setError('Ошибка регистрации. Проверьте введенные данные.');
            }
        } catch (error) {
            setError('Произошла ошибка. Попробуйте позже.');
            console.error('Error:', error)
        }
    }

    return (
        <form onSubmit={handleSubmit}>
            <div>
                <label htmlFor="email">Email:</label>
                <input
                    type="email"
                    id="email"
                    value={email}
                    onChange={(e) => setEmail(e.target.value)}
                    required
                />
            </div>
            <div>
                <label htmlFor="fio">ФИО:</label>
                <input
                    type="text"
                    id="fio"
                    value={fio}
                    onChange={(e) => setFio(e.target.value)}
                    required
                />
            </div>
            <div>
                <label htmlFor="password">Пароль:</label>
                <input
                    type="password"
                    id="password"
                    value={password}
                    onChange={(e) => setPassword(e.target.value)}
                    required
                />
            </div>
            <div>
                <label htmlFor="myRole">Должность:</label>
                <input
                    type="text"
                    id="myRole"
                    value={myRole}
                    onChange={(e) => setMyRole(e.target.value)}
                    required
                />
            </div>
            {error && <div className="error">{error}</div>}
            <button type="submit">Зарегистрироваться</button>
        </form>
    )
}

export default SignupForm