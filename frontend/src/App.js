import React, { useEffect } from "react";
import { BrowserRouter, Link, Route, Routes } from "react-router-dom";
import { useDispatch, useSelector } from "react-redux";
import { setAuthToken, setUser } from "./store/authSlice";
import MainPage from "./components/MainPage/MainPage";
import VacancyPage from "./components/VacansyPage/VacansyPage";
import VacancyPageHr from "./components/VacansyPage/VacansyPageHr";
import ProjectsPage from "./components/ProjectsPage/ProjectsPage";
import Login from "./components/AuthPage/Login";
import Registration from "./components/AuthPage/Registration";
import VacansyInfo from "./components/VacansyPage/VacansyInfo";
import UserProfile from "./components/UserProfile/UserProfile";
import NewsPage from "./components/NewsPage/NewsPage";
import ResumePage from "./components/ResumePage/ResumePage";
import ProjectsPageHr from "./components/ProjectsPage/ProjectsPageHr";
import VacansyInfoHR from "./components/VacansyPage/VacansyInfoHR";

function App() {
  const dispatch = useDispatch();
  const user = useSelector((state) => state.auth.user);

  useEffect(() => {
    const token = localStorage.getItem('authToken');
    const storedUser = localStorage.getItem('user');

    if (token && storedUser) {
      dispatch(setAuthToken(token));
      dispatch(setUser(JSON.parse(storedUser)));
    }
  }, [dispatch]);

  const isHR = user && user.roles && user.roles.includes('hr');

  return (
    <div className="App">
      <Routes>
        <Route path="/" element={<MainPage />} />
        <Route path="/vacansy-page" element={isHR ? <VacancyPageHr isHR={isHR} /> : <VacancyPage />} />
        <Route path="/vacansy-info/:id" element={<VacansyInfo isHR={isHR} />} />
        {isHR != null ? <Route path="/vacansy-info-hr/:id" element={<VacansyInfoHR isHR={isHR} />} /> : ""}
        <Route path="/projects-page" element={isHR ? <ProjectsPageHr /> : <ProjectsPage />} />
        <Route path="/login" element={<Login />} />
        <Route path="/registration" element={<Registration />} />
        {user && <Route path="/profile" element={<UserProfile isHR={isHR} />} />}
        <Route path="/news" element={<NewsPage />} />
        <Route path="/resume" element={<ResumePage />} />
        <Route path="*" element={<h1 style={{ margin: "auto" }}>Вернитесь на <Link to="/">главную страницу</Link></h1>} />
      </Routes>
    </div>
  );
}

export default App;
