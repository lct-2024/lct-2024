import React, { useState } from "react"
import CommandChat from "./components/CommandChat";

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
      <CommandChat />
    </div>
  );
}

export default App;
