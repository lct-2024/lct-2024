import React, { useEffect, useState } from 'react'
import style from './ProjectsPage.module.css'
import Navigation from '../Navigation'
import ProjectsList from './ProjectsList'
import Footer from '../Footer'
import axios from 'axios'

const ProjectsPage = () => {

    const [selectedFilter, setSelectedFilter] = useState("Все");
    const [searchTerm, setSearchTerm] = useState('');
    const [projects, setProjects] = useState([])


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
                    console.log(response.data.result)
                    setProjects(response.data.result);
                }

            } catch (error) {
                console.error('Error fetching data:', error);
            }
        };

        fetchData();
    }, []);

    const [comments, setComments] = useState([
        { name: "Иванов Иван Иванович", text: "Здравствуйте! Я не понимаю есть ли в офисе кошки, не нашел в описании компании." },
        { name: "Егоров Александр Петрович", text: "Здравствуйте! Можно ли совмещать работу с учебой?" }
    ])
    const [newCommentText, setNewCommentText] = useState('');
    const [showInput, setShowInput] = useState(false)

    const handleSearchChange = (e) => {
        setSearchTerm(e.target.value);
    };

    const handleSearchSubmit = () => {
        const filteredProjects = projects.filter(project =>
            project.title.toLowerCase().includes(searchTerm.toLowerCase())
        );
        setProjects(filteredProjects);
    };

    const handleKeyPress = (event) => {
        if (event.key === 'Enter') {
            handleSearchSubmit()
            setSearchTerm("");
        }
    };


    const handleFilterClick = (filter) => {
        setSelectedFilter(filter === selectedFilter ? null : filter);
    };

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

    return (<>
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
                        <input type="text" placeholder='Поиск...' value={searchTerm} onChange={handleSearchChange} onKeyPress={handleKeyPress} />
                        <button>
                            <svg width="25" height="25" viewBox="0 0 25 25" fill="none" xmlns="http://www.w3.org/2000/svg">
                                <path d="M21.5 21.5L17.2 17.2M19.5 11.5C19.5 15.9183 15.9183 19.5 11.5 19.5C7.08172 19.5 3.5 15.9183 3.5 11.5C3.5 7.08172 7.08172 3.5 11.5 3.5C15.9183 3.5 19.5 7.08172 19.5 11.5Z" stroke="white" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                            </svg>
                        </button>
                    </div>
                    <div className={style.filter}>
                        <p className={selectedFilter === 'Все' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Все')}>Все</p>
                        <p className={selectedFilter === 'Финтех' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Финтех')}> Финтех</p>
                        <p className={selectedFilter === 'Госсектор' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Госсектор')}>Госсектор</p>
                        <p className={selectedFilter === 'IT' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('IT')}>IT</p>
                        <p className={selectedFilter === 'Медиа' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Медиа')}>Медиа</p>
                        <p className={selectedFilter === 'Нефть и газ' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Нефть и газ')}>Нефть и газ</p>
                        <p className={selectedFilter === 'Ретейл' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Ретейл')}>Ретейл</p>
                        <p className={selectedFilter === 'Коммуникации' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Коммуникации')}>Коммуникации</p>
                        <p className={selectedFilter === 'Транспорт' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Транспорт')}>Транспорт</p>
                        <p className={selectedFilter === 'Другое' ? style.activeFilter : ''}
                            onClick={() => handleFilterClick('Другое')}>Другое</p>
                    </div>
                </div>
                {projects.length === 0 ? <h1 style={{ margin: "0 auto" }}>Проектов нет</h1> : <ProjectsList projects={projects} />}
                <div>
                    <h2>Интересно узнать больше о проектах?</h2>
                    <h2>Не нашли ответ на свой вопрос? Напишите в комментарии, <br /> чтобы получить ответ:</h2>
                    <div className={style.comments}>
                        {comments.map((comment, i) => {
                            return <div className={style.comment} key={i}>
                                <div>
                                    <h4>{comment.name}</h4>
                                    <p>21.01.24  21.00</p>
                                </div>
                                <p>{comment.text}</p>
                            </div>
                        })}
                        {showInput && (<>
                            <textarea onChange={(e) => setNewCommentText(e.target.value)} value={newCommentText} placeholder='Комментарий...' />
                            <button className={style.btn} onClick={handleCommentSubmit}>
                                Отправить комментарий
                            </button>
                        </>)}
                        {showInput === false && <button className={style.btn} onClick={handleShowInput}>Написать комментарий</button>}
                    </div>
                </div>
            </div>
        </div>
        <Footer />
    </>
    )
}

export default ProjectsPage