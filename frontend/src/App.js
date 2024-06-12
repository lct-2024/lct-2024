import React, { useState } from "react"
import MainPage from "./components/Pages/MainPage";
import { BrowserRouter, Link, Route, Routes } from "react-router-dom";
import VacancyPage from "./components/VacansyPage/VacansyPage";
import ProjectsPage from "./components/ProjectsPage/ProjectsPage";
import Login from "./components/AuthPage/Login";
import Registration from "./components/AuthPage/Registration";
import VacansyInfo from "./components/VacansyPage/VacansyInfo";
import UserProfile from "./components/UserProfile/UserProfile";

function App() {

  const [vacansies, setVacansies] = useState([])
  const [user, setUser] = useState(null);

  return (
    <BrowserRouter>
      <div className="App">
        <Routes>
          <Route path="/" element={<MainPage />} />
          <Route path="/vacansy-page" element={<VacancyPage vacansies={vacansies} setVacansies={setVacansies} />} />
          <Route path="/vacansy-info/:id" element={<VacansyInfo vacansies={vacansies} setVacansies={setVacansies} />} />
          <Route path="/projects-page" element={<ProjectsPage />} />
          <Route path="/login" element={<Login setUser={setUser} />} />
          <Route path="/registration" element={<Registration setUser={setUser} />} />
          {user && <Route path="/profile" element={<UserProfile user={user} />} />}
          <Route path="*" element={<h1 style={{ margin: "auto" }}>Вернитесь на <Link to="/">главную страницу</Link></h1>} />
        </Routes>
      </div>
    </BrowserRouter>
  );
}

export default App;
