import React, { useState } from "react";
import axios from "axios";

const LoginForm = ({ onLogin }) => {
    const [email, setEmail] = useState("");
    const [password, setPassword] = useState("");
    const [error, setError] = useState(null); // Добавляем состояние для ошибки

    const handleSubmit = async (e) => {
        e.preventDefault();
        setError(null); // Очищаем ошибку перед отправкой запроса

        try {
            const response = await axios.post(
                "https://passport.lct24.dev.40ants.com/api/login",
                {
                    jsonrpc: "2.0",
                    method: "login",
                    params: {
                        email,
                        password,
                    },
                    id: 0,
                }
            );

            console.log("Response:", response.data);
            const loginResult = response.data.login_result; // Исправленное имя результата

            if (loginResult === "success") {
                onLogin();
                const token = response.data.token; // Исправленное имя поля для токена
                localStorage.setItem("jwtToken", token);
                // Перенаправление на защищенную страницу
                window.location.href = "/dashboard";
            } else {
                setError("Неправильный email или пароль"); // Обработка ошибки
            }
        } catch (error) {
            setError("Произошла ошибка. Попробуйте позже."); // Обработка ошибки
            console.error("Error:", error);
        }
    };

    return (
        <form onSubmit={handleSubmit}>
            <div>
                <label htmlFor="emailL">Email:</label>
                <input
                    type="email"
                    id="emailL"
                    value={email}
                    onChange={(e) => setEmail(e.target.value)}
                    required
                />
            </div>
            <div>
                <label htmlFor="passwordL">Пароль:</label>
                <input
                    type="password"
                    id="passwordL"
                    value={password}
                    onChange={(e) => setPassword(e.target.value)}
                    required
                />
            </div>
            {error && <div className="error">{error}</div>}
            <button type="submit">Войти</button>
        </form>
    );
};

export default LoginForm;