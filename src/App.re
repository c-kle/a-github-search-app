type repository = {
  name: string,
  description: string,
  href: string,
};

type state = {
  input: string,
  isLoading: bool,
  results: list(repository),
};

type action =
  | UpdateInput(string)
  | Search
  | SearchSuccess(list(repository));

module Api = {
  open Json.Decode;
  let decodeResults =
    field(
      "items",
      list(
        optional(json =>
          {
            name: field("name", string, json),
            description: field("description", string, json),
            href: field("html_url", string, json),
          }
        ),
      ),
    );
  let getResults = query =>
    Js.Promise.(
      Fetch.fetch("https://api.github.com/search/repositories?q=" ++ query)
      |> then_(Fetch.Response.json)
      |> then_(json => decodeResults(json) |> resolve)
      |> then_(results =>
           results
           |> List.filter(optionalItem =>
                switch (optionalItem) {
                | Some(_) => true
                | None => false
                }
              )
           /* Turn our items out of option types into a regular record */
           |> List.map(item =>
                switch (item) {
                | Some(item) => item
                }
              )
           |> resolve
         )
    );
};

[@react.component]
let make = () => {
  let (state, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | UpdateInput(newInput) => {...state, input: newInput}
        | Search => {...state, isLoading: true}
        | SearchSuccess(newResults) => {
            ...state,
            isLoading: false,
            results: newResults,
          }
        },
      {input: "", isLoading: false, results: []},
    );

  let searchRepositories = () => {
    dispatch(Search);
    Api.getResults(state.input)
    |> Js.Promise.then_(results => {
         dispatch(SearchSuccess(results));
         Js.Promise.resolve();
       })
    |> ignore;
  };

  <div>
    <form
      onSubmit={ev => {
        ReactEvent.Form.preventDefault(ev);
        searchRepositories();
      }}>
      <label htmlFor="search"> {ReasonReact.string("Search")} </label>
      <input
        id="search"
        name="search"
        value={state.input}
        onChange={ev => {
          let value = ReactEvent.Form.target(ev)##value;
          dispatch(UpdateInput(value));
        }}
      />
      <button type_="submit"> {ReasonReact.string("Submit Search")} </button>
    </form>
    <div>
      {state.isLoading
         ? ReasonReact.string("Loading...")
         : state.results
           /* Convert to list to an array for ReasonReact's type bindings */
           |> Array.of_list
           /* Map each array item to a <Card /> component */
           |> Array.map(({name, href, description}) =>
                <Card key=href name href description />
              )
           /* Transform the array into a valid React node, similar to ReasonReact.string */
           |> ReasonReact.array}
    </div>
  </div>;
};