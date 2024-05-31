import React, { useState } from "react"
import SignupForm from "./components/SignUpForm";
import UserProfile from "./components/UserProfile";
import LoginForm from "./components/LoginForm";

function App() {

  const [isLoggedIn, setIsLoggedIn] = useState(false);

  const handleLogin = () => {
    setIsLoggedIn(true);
  };

  const handleLogout = () => {
    setIsLoggedIn(false);
  };

  return (
    <div className="App">
      {isLoggedIn ? (
        <div>
          <h1>Добро пожаловать!</h1>
          <UserProfile />
          <button onClick={handleLogout}>Выйти</button>
        </div>
      ) : (
        <div>
          <h2>Регистрация</h2>
          <SignupForm onSignup={handleLogin} />
          <h2>Вход</h2>
          <LoginForm onLogin={handleLogin} />
        </div>
      )}
    </div>
  );
}

export default App;
