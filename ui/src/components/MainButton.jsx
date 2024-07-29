function MainButton(props) {
  return (
    <>
      <div className="btn-group stack-exception">
        <button id={props.id} type="button" className="btn toggle-btn primary">
          {props.name}
        </button>
      </div>
    </>
  );
}

export default MainButton;
