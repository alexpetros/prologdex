window.monSelectListener = () => {
  if (event.key === 'Enter') {
    const value = event.target.value
    document.querySelector('team-list').add(value)
  }
}

class TeamList extends HTMLElement {
  getMon(mon) {
    return this.querySelector(`team-list-item[mon="${mon}"]`)
  }

  add(mon) {
    if (this.getMon(mon)) {
      console.warn(`${mon} is already on the team`)
      return
    }

    const item = new TeamListItem()
    item.setAttribute('mon', mon)
    this.append(item)
  }

  remove(mon) {
    this.getMon(mon).remove()
  }
}
customElements.define('team-list', TeamList)

class TeamListItem extends HTMLElement {
  connectedCallback() {
    this.mon = this.getAttribute('mon')
    this.innerHTML = `
      <span>${this.mon}</span> <button>Remove</button>
    `
    this.querySelector('button').onclick = () => this.remove()
  }
}
customElements.define('team-list-item', TeamListItem)
