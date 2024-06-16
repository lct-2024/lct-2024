import { createSlice, createAsyncThunk } from '@reduxjs/toolkit';
import axios from 'axios';

const API_BASE_URL = 'https://chat.lct24.dev.40ants.com/api';

export const fetchOrCreateChat = createAsyncThunk(
    'comments/fetchOrCreateChat',
    async ({ contentId, contentType }, { getState, rejectWithValue }) => {
        const authToken = getState().auth.token;
        const chatId = getState().comments.chatId;

        const createChat = async (content_id, content_type, title, isPrivate) => {
            const response = await axios.post(`${API_BASE_URL}/create_chat`, {
                jsonrpc: '2.0',
                method: 'create_chat',
                params: { content_id, content_type, title, private: isPrivate },
                id: 1,
            }, {
                headers: { 'Authorization': `${authToken}` }
            });
            console.log(response.data)
            console.log("чат успешно создан")
            return response.data.result;
        };

        try {
            let result;
            if (chatId) {
                result = { chatId };
            } else {
                const newChat = await createChat(contentId, contentType, 'Chat Title', false);
                console.log(newChat)
                result = { chatId: newChat.id };
            }

            // Получаем сообщения чата с использованием chatId
            const messagesResponse = await axios.post(`${API_BASE_URL}/get_messages`, {
                jsonrpc: '2.0',
                method: 'get_messages',
                params: { chat_id: result.chatId, page_key: 0, limit: 10 }, // Пример параметров пагинации
                id: 1,
            }, {
                headers: { 'Authorization': `${authToken}` }
            });

            result.messages = messagesResponse.data.result.items; // Записываем полученные сообщения

            return result;
        } catch (error) {
            if (error.response && error.response.data.error) {
                const newChat = await createChat(contentId, contentType, 'Chat Title', false);
                return { chatId: newChat.id, messages: [] };
            } else {
                return rejectWithValue(error.message);
            }
        }
    }
);

export const postMessage = createAsyncThunk(
    'comments/postMessage',
    async ({ message }, { getState, rejectWithValue }) => {
        const authToken = getState().auth.token;
        const chatId = getState().comments.chatId; // Используем chatId из состояния

        try {
            const response = await axios.post(`${API_BASE_URL}/post`, {
                jsonrpc: '2.0',
                method: 'post',
                params: { chat_id: chatId, message },
                id: 1,
            }, {
                headers: { 'Authorization': `${authToken}` }
            });
            console.log(response.data)
            console.log("сообщения с чата получены" + response.data.result)
            return response.data.result;
        } catch (error) {
            return rejectWithValue(error.message);
        }
    }
);

const commentsSlice = createSlice({
    name: 'comments',
    initialState: {
        chatId: null,
        comments: [],
        status: 'idle',
        error: null,
    },
    reducers: {},
    extraReducers: (builder) => {
        builder
            .addCase(fetchOrCreateChat.pending, (state) => {
                state.status = 'loading';
            })
            .addCase(fetchOrCreateChat.fulfilled, (state, action) => {
                state.status = 'succeeded';
                state.chatId = action.payload.chatId;
                state.comments = action.payload.messages;
            })
            .addCase(fetchOrCreateChat.rejected, (state, action) => {
                state.status = 'failed';
                state.error = action.payload;
            })
            .addCase(postMessage.fulfilled, (state, action) => {
                if (!Array.isArray(state.comments)) {
                    state.comments = [];
                }
                state.comments.push(action.payload);
            })
            .addCase(postMessage.rejected, (state, action) => {
                state.error = action.payload;
            });
    },
});

export default commentsSlice.reducer;
