import React, { useEffect, useState } from 'react';
import { useSelector, useDispatch } from 'react-redux';
import { fetchOrCreateChat, postMessage } from '../store/commentsSlice';
import style from './Comments.module.css';

const Comments = ({ text, contentId, contentType }) => {
    const [newCommentText, setNewCommentText] = useState('');
    const [showInput, setShowInput] = useState(false);
    const user = useSelector((state) => state.auth.user);
    const dispatch = useDispatch();
    const authToken = useSelector((state) => state.auth.token);
    const chatId = useSelector((state) => state.comments.chatId);
    const comments = useSelector((state) => state.comments.comments);
    const status = useSelector((state) => state.comments.status);
    const error = useSelector((state) => state.comments.error);

    useEffect(() => {
        if (authToken) {
            dispatch(fetchOrCreateChat({ contentId, contentType }));
        }
    }, [authToken, dispatch, contentId, contentType]);

    const handleShowInput = () => {
        setShowInput(true);
    };

    const handleCommentSubmit = async () => {
        if (newCommentText.trim() === '') return;

        try {
            dispatch(postMessage({ message: newCommentText }));
            setNewCommentText('');
            setShowInput(false);
        } catch (error) {
            console.error('Error posting comment:', error);
        }
    };

    if (status === 'loading') {
        return <div>Loading...</div>;
    }

    if (status === 'failed') {
        return <div>Error: {error}</div>;
    }

    return (
        <div className='container'>
            <h2 className={style.title}>Интересно узнать больше о {text}?</h2>
            <h2 className={style.title}>Не нашли ответ на свой вопрос? Напишите в комментарии, <br /> чтобы получить ответ:</h2>
            <div className={style.comments}>
                {comments && comments.length > 0 ? (
                    comments.map((comment, i) => (
                        <div className={style.comment} key={i}>
                            <div>
                                <h4>{user.fio}</h4>
                                <p>{new Date(comment.created_at).toLocaleString()}</p>
                            </div>
                            <p>{comment.message}</p>
                        </div>
                    ))
                ) : (
                    <p>Нет комментариев</p>
                )}
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
