import React, { useEffect, useState } from 'react';
import style from './ProjectsPage.module.css';
import Navigation from '../Navigation';
import ProjectsList from './ProjectsList';
import Footer from '../Footer';
import axios from 'axios';
import { useDispatch, useSelector } from 'react-redux';
import { setProjects } from '../../store/projectsSlice';
import Comments from '../Comments';

const ProjectsPage = () => {

    const projects = useSelector(state => state.projects)
    const dispatch = useDispatch()
    const [filteredProjects, setFilteredProjects] = useState([]);
    const [searchTerm, setSearchTerm] = useState('');
    const [selectedFilter, setSelectedFilter] = useState('Все');

    useEffect(() => {
        const fetchData = async () => {
            try {
                const response = await axios.post('https://ats.lct24.dev.40ants.com/api/get_projects', {
                    jsonrpc: '2.0',
                    method: 'get_projects',
                    params: [],
                    id: 1
                });
                if (response.data.error) {
                    console.error('Error fetching data:', response.data.error.message);
                } else {
                    dispatch(setProjects(response.data.result));
                    setFilteredProjects(response.data.result);
                }
            } catch (error) {
                console.error('Error fetching data:', error);
            }
        };

        fetchData();
    }, []);

    useEffect(() => {
        filterProjects();
    }, [searchTerm, selectedFilter, projects]);

    const filterProjects = () => {
        let filtered = [...projects];

        if (searchTerm.trim() !== '') {
            filtered = filtered.filter((project) =>
                project.title.toLowerCase().includes(searchTerm.toLowerCase())
            );
        }

        if (selectedFilter !== 'Все') {
            filtered = filtered.filter((project) =>
                project.themes.some((theme) => theme.title.toLowerCase() === selectedFilter.toLowerCase())
            );
        }

        setFilteredProjects(filtered);
    };

    const handleSearchChange = (event) => {
        setSearchTerm(event.target.value);
    };

    const handleFilterClick = (filter) => {
        setSelectedFilter(filter);
    };

    const handleKeyPress = (event) => {
        if (event.key === 'Enter') {
            filterProjects();
        }
    };

    const [comments, setComments] = useState([
        { name: "Иванов Иван Иванович", text: "Здравствуйте! Я не понимаю есть ли в офисе кошки, не нашел в описании компании." },
        { name: "Егоров Александр Петрович", text: "Здравствуйте! Можно ли совмещать работу с учебой?" }
    ]);
    const [newCommentText, setNewCommentText] = useState('');
    const [showInput, setShowInput] = useState(false);

    const handleShowInput = () => {
        setShowInput(true);
    };

    const handleCommentSubmit = () => {
        setComments([
            ...comments,
            { name: "Николай Семенович", text: newCommentText }
        ]);
        setNewCommentText('');
        setShowInput(false);
    };

    return (
        <>
            <div className={style.main}>
                <div className='container'>
                    <div className={style.head}>
                        <Navigation />
                        <h1>ПРОЕКТЫ</h1>
                    </div>
                </div>
            </div>
            <div className='container'>
                <div className={style.body}>
                    <div>
                        <div className={style.search}>
                            <input
                                type="text"
                                placeholder='Поиск...'
                                value={searchTerm}
                                onChange={handleSearchChange}
                                onKeyPress={handleKeyPress}
                            />
                            <button>
                                <svg width="25" height="25" viewBox="0 0 25 25" fill="none" xmlns="http://www.w3.org/2000/svg">
                                    <path d="M21.5 21.5L17.2 17.2M19.5 11.5C19.5 15.9183 15.9183 19.5 11.5 19.5C7.08172 19.5 3.5 15.9183 3.5 11.5C3.5 7.08172 7.08172 3.5 11.5 3.5C15.9183 3.5 19.5 7.08172 19.5 11.5Z" stroke="white" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" />
                                </svg>
                            </button>
                        </div>
                        <div className={style.filter}>
                            {['Все', 'Финтех', 'Госсектор', 'IT', 'Медиа', 'Нефть и газ', 'Ретейл', 'Коммуникации', 'Транспорт', 'Другое'].map((filter) => (
                                <p
                                    key={filter}
                                    className={selectedFilter === filter ? style.activeFilter : ''}
                                    onClick={() => handleFilterClick(filter)}
                                >
                                    {filter}
                                </p>
                            ))}
                        </div>
                    </div>
                    {filteredProjects.length === 0 ? (
                        <h1 style={{ margin: '0 auto' }}>Проектов нет</h1>
                    ) : (
                        <ProjectsList projects={filteredProjects} />
                    )}
                    <Comments text="проектах" />
                </div>
            </div>
            <Footer />
        </>
    );
};

export default ProjectsPage;
