import React, { useEffect, useState, useMemo } from 'react';
import { useSelector, useDispatch } from 'react-redux';
import { fetchChat, postMessage } from '../store/commentsSlice';
import style from './Comments.module.css';

const Comments = ({ text, chatId }) => {
    const [newCommentText, setNewCommentText] = useState('');
    const [showInput, setShowInput] = useState(false);
    const user = useSelector((state) => state.auth.user || null);
    const dispatch = useDispatch();
    const comments = useSelector((state) => state.comments.comments);
    const status = useSelector((state) => state.comments.status);
    const error = useSelector((state) => state.comments.error);

    useEffect(() => {
        if (chatId) {
            dispatch(fetchChat({ chatId }));
        }
    }, [chatId, dispatch]);

    const handleShowInput = () => {
        setShowInput(true);
    };

    const handleCommentSubmit = async () => {
        if (newCommentText.trim() === '') return;

        try {
            await dispatch(postMessage({ message: newCommentText })).unwrap();
            setNewCommentText('');
            setShowInput(false);
        } catch (error) {
            console.error('Error posting comment:', error);
        }
    };

    const renderComments = useMemo(() => {
        if (comments && comments.length > 0) {
            return comments.map((comment, i) => (
                <div className={style.comment} key={i}>
                    <div>
                        <h4>{user ? (user.fio || "Вы не зарегистрированы") : "Вы не зарегистрированы"}</h4>
                        <p>{new Date(comment.created_at).toLocaleString()}</p>
                    </div>
                    <p>{comment.message}</p>
                </div>
            ));
        } else {
            return <p>Нет комментариев</p>;
        }
    }, [comments, user.fio]);

    const renderLoading = useMemo(() => {
        if (status === 'loading') {
            return <div>Loading...</div>;
        }
        if (status === 'failed') {
            return <div>Error: {error}</div>;
        }
        return null;
    }, [status, error]);

    return (
        <div className='container'>
            <h2 className={style.title}>Интересно узнать больше о {text}?</h2>
            <h2 className={style.title}>Не нашли ответ на свой вопрос? Напишите в комментарии, <br /> чтобы получить ответ:</h2>
            <div className={style.comments}>
                {renderLoading}
                {renderComments}
                {showInput && (
                    <>
                        <textarea
                            onChange={(e) => setNewCommentText(e.target.value)}
                            value={newCommentText}
                            placeholder='Комментарий...'
                        />
                        <button className={style.btn} onClick={handleCommentSubmit}>
                            Отправить комментарий
                        </button>
                    </>
                )}
                {!showInput && <button className={style.btn} onClick={handleShowInput}>Написать комментарий</button>}
            </div>
        </div>
    );
};

export default Comments;