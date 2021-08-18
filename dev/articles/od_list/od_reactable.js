var od_table = {
  activate_tooltips: function (selector = '.od_details [data-tippy-content]') {
    setTimeout(function() { tippy(selector, {theme: 'custom'}) }, 100);
  },
  escapeHTML: function(unsafe) {
    return unsafe
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#039;");
  },
  format_time: function(t) {
    let day = t.getDate(), month = t.getMonth() + 1, year = t.getFullYear()
    if (month.toString().length == 1) month = "0" + month;
    if (day.toString().length == 1) day = "0" + day;
    return day + "." + month + "." + year;
  },
  format_time_compact: function(t) {
    let year = t.getFullYear().toString().substr(2, 2), month = t.getMonth(),
      month_name = ["Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul",
         "Aug", "Sep", "Okt", "Nov", "Dez"][month]
    return month_name + "'" + year;
  },
  parse_time: function(cellInfo) {
    let time_value = new Date(cellInfo.value),
      time_tooltip = od_table.format_time(time_value),
      time_pretty = od_table.format_time_compact(time_value);
    od_table.activate_tooltips('.reactable [data-tippy-content]');
    return `<span data-tippy-content="${time_tooltip}">${time_pretty}</span>`;
  },
  details: {
    fields: function(rowInfo) {
      let row = rowInfo.row, fields = row.fields, od_id = row.id_od,
        classifications = "";
      for (i in fields.code) {
        if (i > 0)
          classifications += ", "
        let code = fields.code[i], label = fields.label[i],
          url = `https://data.statistik.gv.at/data/${od_id}_${code}.csv`
        classifications +=
          `<a href="${url}" target="_blank" data-tippy-content="${code}">${label}</a>`
      }
      od_table.activate_tooltips();

      return `<div class="od_details"><b>Klassifikationen</b>: ${classifications}</div>`
    },
    measures: function(rowInfo) {
      let row = rowInfo.row, measures = row.measures, names = measures.label,
        msrs = "", nchar = 0;
      for (i in names) {
        if (i > 0)
          msrs += ", "
        nchar += names[i].length + 2
        msrs += `<u data-tippy-content="${measures.code[i]}">
          ${od_table.escapeHTML(names[i])}</u>`
        if (nchar > 400)
          break;
      }
      if (i < names.length-1)
        msrs += ", ..."
      od_table.activate_tooltips();
      return `<div class="od_details"><b>Messwerte</b>: ${msrs}</div>`
    },
    last_modified: function(rowInfo) {
      let row = rowInfo.row,
        created = od_table.format_time(new Date(row.created)),
        modified = od_table.format_time(new Date(row.last_modified)),
        url = `https://data.statistik.gv.at/data/${row.id_od}.csv`;
      return `
        <div class="od_details">
          Dieser Datensatz wurde am ${created} erstellt und zuletzt am
          ${modified} aktualisiert. <br/>
          Aktualisierungsintervall: ${row.update_frequency} <br/><br/>
          Datensatz: <a href="${url}" target = "_blank">${row.id_od}.csv</a>
        </div>`;
    },
    label: function(rowInfo) {
      let row = rowInfo.row, sc_id = row.id_sc, od_id = row.id_od,
        sc_url = "https://statcube.at/statcube", od_url = "https://data.statistik.gv.at";
      if (sc_id === null)
        sc_render = ""
      else
        sc_render = `<br/>
          STATcube: <code>${sc_id}</code>.
          <a href="${sc_url}/opendatabase?id=${sc_id}" target="_blank">Tabellenansicht</a>
          <a href="${sc_url}/openinfopage?id=${sc_id}" target="_blank">Info-Seite</a>`
      desc = ""
      if (row.description != row.label)
        desc = row.description + '<br/><br/>'
      return `
        <div class="od_details">
          ${desc}
          open.data: <code>${od_id}</code>.
          <a href="${od_url}/web/meta.jsp?dataset=${od_id}" target="_blank">Metadaten</a>
          <a href="${od_url}/ogd/json?dataset=${od_id}" target="blank">JSON</a>
          ${sc_render}
        </div>`
    },
    category: function(rowInfo) {
      let row = rowInfo.row, tags = "", categories = "";
      for (i in row.tags) {
        if (i > 0)
          tags += ", "
        tags += row.tags[i];
      }
      for (i in row.categories) {
        if (i > 0)
          categories += ", "
        categories += row.categories[i];
      }
      return `<div class="od_details">
                <b>Kategorien</b>: ${categories}<br/>
                <b>Schlagworte</b>: ${tags}
              </div>`;
    }
  }
}
