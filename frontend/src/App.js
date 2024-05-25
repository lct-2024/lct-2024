import React, { useState } from "react"
import SignupForm from "./components/SignUpForm";
import UserProfile from "./components/UserProfile";

function App() {
  return (
    <div className="App">
      <SignupForm />
      <UserProfile />
    </div>
  );
}

export default App;
