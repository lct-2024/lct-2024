import React, { useEffect } from 'react';
import Footer from '../Footer';
import Navigation from '../Navigation';
import style from "./NewsPage.module.css";
import { useSelector, useDispatch } from 'react-redux';
import { fetchNews } from '../../store/newsSlice';
import Comments from '../Comments';

const NewsPage = () => {
    const news = useSelector((state) => state.news.data);
    const status = useSelector((state) => state.news.status);
    const error = useSelector((state) => state.news.error);
    const chatId = news ? news.chat_id : null;
    const dispatch = useDispatch();

    useEffect(() => {
        if (status === 'idle') {
            dispatch(fetchNews());
        }
    }, [status, dispatch]);

    if (status === 'loading') {
        return <div>Loading...</div>;
    }

    if (status === 'failed') {
        return <div>Error: {error}</div>;
    }

    return (
        <section className={style.sec}>
            <div className={style.main}>
                <div className='container'>
                    <div className={style.head}>
                        <Navigation />
                        <h1>НОВОСТИ</h1>
                        <p>Делимся тем, кем являемся, а еще знаниями и событиями</p>
                    </div>
                </div>
            </div>
            <div className={style.body}>
                <div className="container">
                    <div className={style.news}>
                        {news.map((item, i) => (
                            <div className={style.newsBlock} key={i}>
                                <img src={item.image} alt="фото новости" />
                                <div className={style.about}>
                                    <h4>{item.short_title}</h4>
                                    <p dangerouslySetInnerHTML={{ __html: item.short_html }}></p>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>
            <Comments contentId='newsId' text="новостях" chatId={chatId} contentType='news' />
            <Footer />
        </section>
    );
};

export default NewsPage;
