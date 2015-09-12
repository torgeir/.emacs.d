/*global immstruct component React el*/
var data = immstruct({
  items: [
    { checked: true, text: 'Create example an Omniscient playground! :D' },
    { checked: true, text: 'Make it support es6!' },
    { checked: true, text: 'And jsx!' },
    { checked: true, text: 'It should compile as you type!' },
    { checked: true, text: 'And give immediate feedback!' },
    { checked: false, text: 'Make more examples!!1' }
  ]
});

// check your console to see omniscient's debug info for what needs
// to render as the immstruct structure change!
component.debug();

var Item = component('Item', function ({item}) {
  var onChecked = () => item.update('checked', state => !state);
  var style = {
    textDecoration: item.get('checked') ? 'line-through' : 'none'
    // color: item.get('checked') ? 'green' : ''
  };
  return (
      <label style={style}>
      <input type="checkbox" onChange={onChecked} checked={item.get('checked')} />
      {item.get('text')}
    </label>
  );
}).jsx;

Item = Item;

var List = component('List', ({items}) =>
                     <form>
                     <ul>
                     {items.toArray().map((item, i) =>
                                          <li key={i}>
                                          <Item item={item} />
                                          </li>
                                         )}
                     </ul>
                     </form>);

render();
data.on('swap', render);



