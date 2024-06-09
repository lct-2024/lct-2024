import React, { useState } from "react"
import MainPage from "./components/Pages/MainPage";
import { BrowserRouter, Route, Routes } from "react-router-dom";
import VacancyPage from "./components/VacansyPage/VacansyPage";
import ProjectsPage from "./components/ProjectsPage/ProjectsPage";
import Login from "./components/AuthPage/Login";
import Registration from "./components/AuthPage/Registration";
import VacansyInfo from "./components/VacansyPage/VacansyInfo";

function App() {

  const [vacansies, setVacansies] = useState([])

  return (
    <BrowserRouter>
      <div className="App">
        <Routes>
          <Route path="/" element={<MainPage />} />
          <Route path="/vacansy-page" element={<VacancyPage vacansies={vacansies} setVacansies={setVacansies} />} />
          <Route path="/vacansy-info" element={<VacansyInfo vacansies={vacansies} setVacansies={setVacansies} />} />
          <Route path="/projects-page" element={<ProjectsPage />} />
          <Route path="/login" element={<Login />} />
          <Route path="/registration" element={<Registration />} />
        </Routes>
      </div>
    </BrowserRouter>
  );
}

export default App;
