// Generated by CoffeeScript 1.11.1
var altpink, backbutton, backbuttong, backbuttontext, bgcolor, bigCircRad, bigRad, black, buttonh, buttonw, buttonw2, darkBlue, darkGray, darkRed, draw, drawRandom, effpanel, h, labelcolor, left, lightGray, lodpanel, maincolor, medCircRad, nodig, onedig, pad, peakRad, permbutton, permbuttong, pink, purple, smCircRad, svg, tinyRad, titlecolor, totalh, totalw, twodig, w;

darkBlue = "darkslateblue";

lightGray = d3.rgb(230, 230, 230);

darkGray = d3.rgb(200, 200, 200);

pink = "hotpink";

altpink = "#E9CFEC";

purple = "#8C4374";

darkRed = "crimson";

bgcolor = "white";

black = "black";

labelcolor = "black";

titlecolor = "darkslateblue";

maincolor = "darkslateblue";

nodig = d3.format(".0f");

onedig = d3.format(".1f");

twodig = d3.format(".2f");

peakRad = 2;

bigRad = 5;

w = [800, 200];

h = 500;

pad = {
  left: 60,
  top: 60,
  right: 10,
  bottom: 60,
  inner: 5
};

totalw = w[0] + w[1] + pad.left * 2 + pad.right * 2;

totalh = h + pad.top + pad.bottom;

left = [pad.left, 2 * pad.left + w[0] + pad.right];

bigCircRad = "4";

medCircRad = "4";

smCircRad = "2";

tinyRad = "1";

buttonw = 170;

buttonh = 40;

totalh += buttonh + pad.bottom / 2;

buttonw2 = 80;

svg = d3.select("div#figure").append("svg").attr("height", totalh).attr("width", totalw);

lodpanel = svg.append("g").attr("id", "random_lodpanel");

effpanel = svg.append("g").attr("id", "random_effpanel");

permbuttong = svg.append("g").attr("id", "random_permutebutton").attr("transform", "translate(" + pad.left + "," + (totalh - buttonh - pad.bottom / 2) + ")");

permbutton = permbuttong.append("rect").attr("x", 0).attr("y", 0).attr("width", buttonw).attr("height", buttonh).attr("fill", d3.rgb(152, 254, 152)).attr("stroke", black).attr("stroke-width", 1);

permbuttong.append("text").attr("x", buttonw / 2).attr("y", buttonh / 2).attr("text-anchor", "middle").attr("dominant-baseline", "middle").text("Randomize!").style("font-size", "28px").style("pointer-events", "none").attr("fill", "black");

backbuttong = svg.append("g").attr("id", "random_backbutton").attr("transform", "translate(" + (pad.left + buttonw + buttonw2 / 2) + "," + (totalh - buttonh - pad.bottom / 2) + ")");

backbutton = backbuttong.append("rect").attr("x", 0).attr("y", 0).attr("width", buttonw2).attr("height", buttonh).attr("fill", d3.rgb(254, 152, 254)).attr("stroke", black).attr("stroke-width", 1).attr("opacity", 0);

backbuttontext = backbuttong.append("text").attr("x", buttonw2 / 2).attr("y", buttonh / 2).attr("text-anchor", "middle").attr("dominant-baseline", "middle").text("Back").style("font-size", "28px").style("pointer-events", "none").attr("fill", black).attr("opacity", 0);

draw = function(data) {
  var col;
  col = 0;
  drawRandom(data, col);
  permbutton.on("click", function() {
    if (col < data.phevals.length - 1) {
      col++;
    }
    if (col > 0) {
      backbutton.transition().duration(250).attr("opacity", 1);
      backbuttontext.transition().duration(250).attr("opacity", 1);
    }
    lodpanel.remove();
    effpanel.remove();
    lodpanel = svg.append("g").attr("id", "random_lodpanel");
    effpanel = svg.append("g").attr("id", "random_effpanel");
    return drawRandom(data, col);
  });
  return backbutton.on("click", function() {
    if (col > 0) {
      col--;
    }
    lodpanel.remove();
    effpanel.remove();
    lodpanel = svg.append("g").attr("id", "random_lodpanel");
    effpanel = svg.append("g").attr("id", "random_effpanel");
    if (col === 0) {
      d3.select(this).transition().duration(250).attr("opacity", 0);
      backbuttontext.transition().duration(250).attr("opacity", 0);
    }
    return drawRandom(data, col);
  });
};

drawRandom = function(data, column) {
  var average, chr, chrColor, chrEnd, chrGap, chrLength, chrPixelEnd, chrPixelStart, chrRect, chrStart, cur, curves, dotsAtMarkers, effaxes, effticks, efftip, effxScale, effxScaleXchr, effyScale, i, index, indtip, j, jitter, jitterAmount, k, l, lastMarker, len, len1, len2, len3, len4, len5, len6, len7, len8, len9, lod, lodaxes, lodcurve, lodticks, lodxScale, lodyScale, m, markerClick, markerchr, martip, maxLod, maxLodByChr, maxLodByChr_marker, maxLod_marker, maxPhe, minPhe, o, p, plotPXG, pos, q, r, ref, ref1, ref2, ref3, ref4, ref5, ref6, ref7, ref8, ref9, revPXG, s, t, title, tmp, totalChrLength, u, xloc, yloc;
  minPhe = d3.min(data.phevals[column]);
  maxPhe = d3.max(data.phevals[column]);
  maxLod = 0;
  maxLod_marker = "";
  maxLodByChr = {};
  maxLodByChr_marker = {};
  tmp = 0;
  ref = data.chr;
  for (j = 0, len = ref.length; j < len; j++) {
    chr = ref[j];
    maxLodByChr[chr] = 0;
    maxLodByChr_marker[chr] = "";
    ref1 = data.lod[chr].lod[column];
    for (k = 0, len1 = ref1.length; k < len1; k++) {
      lod = ref1[k];
      if (maxLod < lod) {
        maxLod = lod;
      }
    }
    for (m in data.markerindex[chr]) {
      lod = data.lod[chr].lod[column][data.markerindex[chr][m]];
      if (lod > maxLodByChr[chr]) {
        maxLodByChr[chr] = lod;
        maxLodByChr_marker[chr] = m;
      }
      if (lod > tmp) {
        tmp = lod;
        maxLod_marker = m;
      }
    }
  }
  jitterAmount = w[1] / 40;
  jitter = [];
  for (i in data.phevals[column]) {
    jitter[i] = data.jitter[i] * jitterAmount * 2;
  }
  lodpanel.append("rect").attr("x", pad.left).attr("y", pad.top).attr("height", h).attr("width", w[0]).attr("fill", lightGray).attr("stroke", black).attr("stroke-width", 1);
  effpanel.append("rect").attr("x", pad.left * 2 + pad.right + w[0]).attr("y", pad.top).attr("height", h).attr("width", w[1]).attr("fill", lightGray).attr("stroke", black).attr("stroke-width", 1);
  chrStart = {};
  chrEnd = {};
  chrLength = {};
  totalChrLength = 0;
  ref2 = data.chr;
  for (l = 0, len2 = ref2.length; l < len2; l++) {
    chr = ref2[l];
    chrStart[chr] = d3.min(data.lod[chr].pos);
    chrEnd[chr] = d3.max(data.lod[chr].pos);
    chrLength[chr] = chrEnd[chr] - chrStart[chr];
    totalChrLength += chrLength[chr];
  }
  chrPixelStart = {};
  chrPixelEnd = {};
  chrGap = 10;
  cur = Math.round(chrGap / 2) + pad.left;
  ref3 = data.chr;
  for (o = 0, len3 = ref3.length; o < len3; o++) {
    chr = ref3[o];
    chrPixelStart[chr] = cur;
    chrPixelEnd[chr] = cur + Math.round((w[0] - chrGap * data.chr.length) / totalChrLength * chrLength[chr]);
    cur = chrPixelEnd[chr] + chrGap;
  }
  lodyScale = d3.scale.linear().domain([0, 18]).range([pad.top + h - pad.inner, pad.top + pad.inner]);
  effyScale = d3.scale.linear().domain([minPhe, maxPhe]).range([pad.top + h - pad.inner, pad.top + pad.inner]);
  effxScale = d3.scale.ordinal().domain([1, 2, 3]).rangePoints([left[1], left[1] + w[1]], 1);
  effxScaleXchr = d3.scale.ordinal().domain([1, 2, 3, 4, 5, 6]).rangePoints([left[1], left[1] + w[1]], 1);
  lodxScale = {};
  chrColor = {};
  ref4 = data.chr;
  for (p = 0, len4 = ref4.length; p < len4; p++) {
    chr = ref4[p];
    lodxScale[chr] = d3.scale.linear().domain([chrStart[chr], chrEnd[chr]]).range([chrPixelStart[chr], chrPixelEnd[chr]]);
    if (chr % 2) {
      chrColor[chr] = lightGray;
    } else {
      chrColor[chr] = darkGray;
    }
  }
  average = function(x) {
    var len5, q, sum, xv;
    sum = 0;
    for (q = 0, len5 = x.length; q < len5; q++) {
      xv = x[q];
      sum += xv;
    }
    return sum / x.length;
  };
  markerchr = {};
  ref5 = data.chr;
  for (q = 0, len5 = ref5.length; q < len5; q++) {
    chr = ref5[q];
    ref6 = data.markers[chr];
    for (r = 0, len6 = ref6.length; r < len6; r++) {
      m = ref6[r];
      markerchr[m] = chr;
    }
  }
  chrRect = lodpanel.append("g").attr("id", "random_chrRect").selectAll("empty").data(data.chr).enter().append("rect").attr("id", function(d) {
    return "random_rect" + d;
  }).attr("x", function(d) {
    return chrPixelStart[d] - chrGap / 2;
  }).attr("y", pad.top).attr("width", function(d) {
    return chrPixelEnd[d] - chrPixelStart[d] + chrGap;
  }).attr("height", h).attr("fill", function(d) {
    return chrColor[d];
  }).attr("stroke", "none");
  lodaxes = lodpanel.append("g").attr("id", "random_lodaxes");
  lodticks = lodyScale.ticks(5);
  lodaxes.append("g").attr("id", "random_lod_yaxis_lines").selectAll("empty").data(lodticks).enter().append("line").attr("x1", pad.left).attr("x2", pad.left + w[0]).attr("y1", function(d) {
    return lodyScale(d);
  }).attr("y2", function(d) {
    return lodyScale(d);
  }).attr("stroke", labelcolor).attr("stroke-width", 1);
  lodaxes.append("g").attr("id", "random_lod_yaxis_labels").selectAll("empty").data(lodticks).enter().append("text").attr("x", pad.left * 0.7).attr("y", function(d) {
    return lodyScale(d);
  }).text(function(d) {
    return nodig(d);
  }).attr("fill", labelcolor).attr("dominant-baseline", "middle");
  xloc = pad.left * 0.4;
  yloc = pad.top + h / 2;
  lodaxes.append("text").attr("id", "random_lod_yaxis_title").attr("x", xloc).attr("y", yloc).text("LOD score").attr("transform", "rotate(270, " + xloc + ", " + yloc + ")").attr("fill", titlecolor).attr("text-anchor", "middle");
  lodaxes.append("g").attr("id", "random_lod_xaxis_labels").selectAll("empty").data(data.chr).enter().append("text").text(function(d) {
    return d;
  }).attr("x", function(d) {
    return (chrPixelStart[d] + chrPixelEnd[d]) / 2;
  }).attr("y", pad.top + h + pad.bottom * 0.25).attr("fill", labelcolor).attr("text-anchor", "middle").attr("dominant-baseline", "hanging");
  lodaxes.append("text").attr("id", "random_lod_xaxis_title").text("Chromosome").attr("x", pad.left + w[0] / 2).attr("y", pad.top + h + pad.bottom * 0.65).attr("text-anchor", "middle").attr("fill", titlecolor).attr("dominant-baseline", "hanging");
  effaxes = effpanel.append("g").attr("id", "random_effaxes");
  effticks = effyScale.ticks(7);
  effaxes.append("g").attr("id", "random_eff_xaxis_lines").selectAll("empty").data([1, 2, 3]).enter().append("line").attr("x1", function(d) {
    return effxScale(d);
  }).attr("x2", function(d) {
    return effxScale(d);
  }).attr("y1", pad.top).attr("y2", pad.top + h).attr("stroke", darkGray).attr("stroke-width", 1);
  effaxes.append("g").attr("id", "random_eff_xaxis_labels").selectAll("empty").data([1, 2, 3]).enter().append("text").attr("x", function(d) {
    return effxScale(d);
  }).attr("y", pad.top + h + pad.bottom * 0.25).text(function(d) {
    return ["GG", "GW", "WW"][d - 1];
  }).attr("fill", labelcolor).attr("text-anchor", "middle").attr("dominant-baseline", "hanging");
  effaxes.append("text").attr("id", "random_lod_xaxis_title").text("Genotype").attr("x", left[1] + w[1] / 2).attr("y", pad.top + h + pad.bottom * 0.65).attr("text-anchor", "middle").attr("fill", titlecolor).attr("dominant-baseline", "hanging");
  effaxes.append("g").attr("id", "random_eff_yaxis_lines").selectAll("empty").data(effticks).enter().append("line").attr("x1", left[1]).attr("x2", left[1] + w[1]).attr("y1", function(d) {
    return effyScale(d);
  }).attr("y2", function(d) {
    return effyScale(d);
  }).attr("stroke", labelcolor).attr("stroke-width", 1);
  effaxes.append("g").attr("id", "random_eff_yaxis_labels").selectAll("empty").data(effticks).enter().append("text").attr("x", left[1] - pad.left * 0.25).attr("y", function(d) {
    return effyScale(d);
  }).text(function(d) {
    return nodig(d);
  }).attr("fill", labelcolor).attr("text-anchor", "end").attr("dominant-baseline", "middle");
  xloc = left[1] - pad.left * 0.7;
  yloc = pad.top + h / 2;
  effaxes.append("text").attr("id", "random_eff_yaxis_title").attr("x", xloc).attr("y", yloc).text(data.phenotype).attr("transform", "rotate(270, " + xloc + ", " + yloc + ")").attr("fill", titlecolor).attr("text-anchor", "right");
  lodcurve = function(chr) {
    return d3.svg.line().x(function(d) {
      return lodxScale[chr](d);
    }).y(function(d, i) {
      return lodyScale(data.lod[chr].lod[column][i]);
    });
  };
  curves = lodpanel.append("g").attr("id", "random_curves");
  dotsAtMarkers = lodpanel.append("g").attr("id", "random_dotsAtMarkers");
  markerClick = {};
  ref7 = data.chr;
  for (s = 0, len7 = ref7.length; s < len7; s++) {
    chr = ref7[s];
    ref8 = data.markers[chr];
    for (t = 0, len8 = ref8.length; t < len8; t++) {
      m = ref8[t];
      markerClick[m] = 0;
    }
  }
  lastMarker = maxLod_marker;
  markerClick[maxLod_marker] = 1;
  martip = d3.svg.tip().orient("right").padding(3).text(function(z) {
    return z;
  }).attr("class", "d3-tip").attr("id", "random_martip");
  indtip = d3.svg.tip().orient("right").padding(3).text(function(d, i) {
    return data.individuals[i];
  }).attr("class", "d3-tip").attr("id", "random_indtip");
  efftip = d3.svg.tip().orient("right").padding(3).text(function(d) {
    return onedig(d);
  }).attr("class", "d3-tip").attr("id", "random_efftip");
  effpanel.append("text").attr("id", "random_pxgtitle_marker").attr("x", left[1] + w[1] / 2).attr("y", pad.top * 0.15).text("").attr("fill", titlecolor).attr("text-anchor", "middle").attr("dominant-baseline", "hanging");
  effpanel.append("text").attr("id", "random_pxgtitle_position").attr("x", left[1] + w[1] / 2).attr("y", pad.top * 0.52).text("").attr("fill", labelcolor).attr("text-anchor", "middle").attr("dominant-baseline", "hanging");
  plotPXG = function(marker) {
    var g, means, n, n_g;
    means = [0, 0, 0, 0, 0, 0];
    n = [0, 0, 0, 0, 0, 0];
    n_g = 0;
    for (i in data.geno[marker]) {
      g = Math.abs(data.geno[marker][i]);
      means[g - 1] += data.phevals[column][i];
      n[g - 1]++;
      if (g > n_g) {
        n_g = g;
      }
    }
    for (i in means) {
      if (n[i] === 0) {
        means[i] = 0;
      } else {
        means[i] /= n[i];
      }
    }
    effaxes.select("g#random_eff_xaxis_lines").remove();
    effaxes.select("g#random_eff_xaxis_labels").remove();
    effaxes.append("g").attr("id", "random_eff_xaxis_lines").selectAll("empty").data(means).enter().append("line").attr("x1", function(d, i) {
      if (i >= n_g || n_g > 3) {
        return effxScaleXchr(i + 1);
      }
      return effxScale(i + 1);
    }).attr("x2", function(d, i) {
      if (i >= n_g || n_g > 3) {
        return effxScaleXchr(i + 1);
      }
      return effxScale(i + 1);
    }).attr("opacity", function(d, i) {
      if (i >= n_g) {
        return 0;
      }
      return 1;
    }).attr("y1", pad.top).attr("y2", pad.top + h).attr("stroke", darkGray).attr("stroke-width", 1);
    effaxes.append("g").attr("id", "random_eff_xaxis_labels").selectAll("empty").data(means).enter().append("text").attr("x", function(d) {
      return effxScale(d);
    }).attr("x", function(d, i) {
      if (i >= n_g || n_g > 3) {
        return effxScaleXchr(i + 1);
      }
      return effxScale(i + 1);
    }).attr("y", pad.top + h + pad.bottom * 0.25).text(function(d, i) {
      if (i >= n_g || n_g > 3) {
        return ["GG", "GWf", "GWr", "WW", "GY", "WY"][i];
      }
      return ["GG", "GW", "WW"][i];
    }).attr("opacity", function(d, i) {
      if (i >= n_g) {
        return 0;
      }
      return 1;
    }).attr("fill", labelcolor).attr("text-anchor", "middle").attr("dominant-baseline", "hanging");
    effpanel.append("g").attr("id", "random_plotPXG").selectAll("empty").data(data.phevals[column]).enter().append("circle").attr("class", "random_plotPXG").attr("cx", function(d, i) {
      g = Math.abs(data.geno[marker][i]);
      if (n_g > 3) {
        return effxScaleXchr(g) + jitter[i];
      }
      return effxScale(g) + jitter[i];
    }).attr("cy", function(d) {
      return effyScale(d);
    }).attr("r", peakRad).attr("fill", function(d, i) {
      if (data.geno[marker][i] < 0) {
        return pink;
      }
      return darkGray;
    }).attr("stroke", function(d, i) {
      if (data.geno[marker][i] < 0) {
        return purple;
      }
      return black;
    }).attr("stroke-width", function(d, i) {
      if (data.geno[marker][i] < 0) {
        return "2";
      }
      return "1";
    }).on("mouseover", function(d, i) {
      d3.select(this).attr("r", bigRad);
      return indtip.call(this, d, i);
    }).on("mouseout", function() {
      d3.selectAll("#random_indtip").remove();
      return d3.select(this).attr("r", peakRad);
    });
    return effpanel.append("g").attr("id", "random_means").selectAll("empty").data(means).enter().append("line").attr("class", "random_plotPXG").attr("x1", function(d, i) {
      if (i >= n_g || n[i] === 0 || n_g > 3) {
        return effxScaleXchr(i + 1) - jitterAmount * 3;
      }
      return effxScale(i + 1) - jitterAmount * 4;
    }).attr("x2", function(d, i) {
      if (i >= n_g || n[i] === 0 || n_g > 3) {
        return effxScaleXchr(i + 1) + jitterAmount * 3;
      }
      return effxScale(i + 1) + jitterAmount * 4;
    }).attr("y1", function(d, i) {
      if (i >= n_g || n[i] === 0) {
        return effyScale(means[2]);
      }
      return effyScale(d);
    }).attr("y2", function(d, i) {
      if (i >= n_g || n[i] === 0) {
        return effyScale(means[2]);
      }
      return effyScale(d);
    }).attr("opacity", function(d, i) {
      if (i >= n_g || n[i] === 0) {
        return 0;
      } else {
        return 1;
      }
    }).attr("stroke", darkBlue).attr("stroke-width", 4).attr("fill", "none").on("mouseover", efftip).on("mouseout", function() {
      return d3.selectAll("#random_efftip").remove();
    });
  };
  revPXG = function(marker) {
    var g, means, n, n_g;
    means = [0, 0, 0, 0, 0, 0];
    n = [0, 0, 0, 0, 0, 0];
    n_g = 0;
    for (i in data.geno[marker]) {
      g = Math.abs(data.geno[marker][i]);
      means[g - 1] += data.phevals[column][i];
      n[g - 1]++;
      if (g > n_g) {
        n_g = g;
      }
    }
    for (i in means) {
      if (n[i] === 0) {
        means[i] = 0;
      } else {
        means[i] /= n[i];
      }
    }
    effaxes.select("g#random_eff_xaxis_lines").remove();
    effaxes.select("g#random_eff_xaxis_labels").remove();
    effaxes.append("g").attr("id", "random_eff_xaxis_lines").selectAll("empty").data(means).enter().append("line").attr("x1", function(d, i) {
      if (i >= n_g || n_g > 3) {
        return effxScaleXchr(i + 1);
      }
      return effxScale(i + 1);
    }).attr("x2", function(d, i) {
      if (i >= n_g || n_g > 3) {
        return effxScaleXchr(i + 1);
      }
      return effxScale(i + 1);
    }).attr("opacity", function(d, i) {
      if (i >= n_g) {
        return 0;
      }
      return 1;
    }).attr("y1", pad.top).attr("y2", pad.top + h).attr("stroke", darkGray).attr("stroke-width", 1);
    effaxes.append("g").attr("id", "random_eff_xaxis_labels").selectAll("empty").data(means).enter().append("text").attr("x", function(d, i) {
      if (i >= n_g || n_g > 3) {
        return effxScaleXchr(i + 1);
      }
      return effxScale(i + 1);
    }).attr("y", pad.top + h + pad.bottom * 0.25).text(function(d, i) {
      if (i >= n_g || n_g > 3) {
        return ["GG", "GWf", "GWr", "WW", "GY", "WY"][i];
      }
      return ["GG", "GW", "WW"][i];
    }).attr("opacity", function(d, i) {
      if (i >= n_g) {
        return 0;
      }
      return 1;
    }).attr("fill", labelcolor).attr("text-anchor", "middle").attr("dominant-baseline", "hanging");
    effpanel.selectAll("line.random_plotPXG").data(means).transition().duration(1000).attr("x1", function(d, i) {
      if (i >= n_g || n[i] === 0 || n_g > 3) {
        return effxScaleXchr(i + 1) - jitterAmount * 3;
      }
      return effxScale(i + 1) - jitterAmount * 4;
    }).attr("x2", function(d, i) {
      if (i >= n_g || n[i] === 0 || n_g > 3) {
        return effxScaleXchr(i + 1) + jitterAmount * 3;
      }
      return effxScale(i + 1) + jitterAmount * 4;
    }).attr("y1", function(d, i) {
      if (i >= n_g || n[i] === 0) {
        return effyScale(means[2]);
      }
      return effyScale(d);
    }).attr("y2", function(d, i) {
      if (i >= n_g || n[i] === 0) {
        return effyScale(means[2]);
      }
      return effyScale(d);
    }).attr("opacity", function(d, i) {
      if (i >= n_g || n[i] === 0) {
        return 0;
      } else {
        return 1;
      }
    });
    return svg.selectAll("circle.random_plotPXG").data(data.phevals[column]).transition().duration(1000).attr("cx", function(d, i) {
      g = Math.abs(data.geno[marker][i]);
      if (n_g > 3) {
        return effxScaleXchr(g) + jitter[i];
      }
      return effxScale(g) + jitter[i];
    }).attr("fill", function(d, i) {
      if (data.geno[marker][i] < 0) {
        return pink;
      }
      return darkGray;
    }).attr("stroke", function(d, i) {
      if (data.geno[marker][i] < 0) {
        return purple;
      }
      return black;
    }).attr("stroke-width", function(d, i) {
      if (data.geno[marker][i] < 0) {
        return "2";
      }
      return "1";
    });
  };
  ref9 = data.chr;
  for (u = 0, len9 = ref9.length; u < len9; u++) {
    chr = ref9[u];
    curves.append("path").datum(data.lod[chr].pos).attr("d", lodcurve(chr)).attr("class", "thickline").attr("stroke", darkBlue).attr("fill", "none").attr("stroke-width", 2).style("pointer-events", "none");
    dotsAtMarkers.selectAll("empty").data(data.markers[chr]).enter().append("circle").attr("cx", function(d) {
      return lodxScale[chr](data.lod[chr].pos[data.markerindex[chr][d]]);
    }).attr("cy", function(d) {
      return lodyScale(data.lod[chr].lod[column][data.markerindex[chr][d]]);
    }).attr("r", tinyRad).attr("fill", pink).attr("stroke", "none").attr("opacity", "0");
    dotsAtMarkers.selectAll("empty").data(data.markers[chr]).enter().append("circle").attr("class", "random_markerCircle").attr("id", function(d) {
      return "random_circ" + markerchr[d] + "_" + data.markerindex[markerchr[d]][d];
    }).attr("cx", function(d) {
      return lodxScale[chr](data.lod[chr].pos[data.markerindex[chr][d]]);
    }).attr("cy", function(d) {
      return lodyScale(data.lod[chr].lod[column][data.markerindex[chr][d]]);
    }).attr("r", bigCircRad).attr("fill", purple).attr("stroke", "none").attr("opacity", 0).on("mouseover", function(d) {
      if (!markerClick[d]) {
        d3.select(this).attr("opacity", 1);
      }
      return martip.call(this, d);
    }).on("mouseout", function(d) {
      d3.select(this).attr("opacity", markerClick[d]);
      return d3.selectAll("#random_martip").remove();
    }).on("click", function(d) {
      var index, lastchr, lastindex, pos, title;
      chr = markerchr[d];
      index = data.markerindex[chr][d];
      pos = data.lod[chr].pos[index];
      title = "(chr " + chr + ", " + (onedig(pos)) + " cM)";
      d3.selectAll("text#random_pxgtitle_marker").text(d);
      d3.selectAll("text#random_pxgtitle_position").text(title);
      markerClick[lastMarker] = 0;
      lastchr = markerchr[lastMarker];
      lastindex = data.markerindex[lastchr][lastMarker];
      d3.select("circle#random_circ" + lastchr + "_" + lastindex).attr("opacity", 0).attr("fill", purple).attr("stroke", "none");
      revPXG(d);
      lastMarker = d;
      markerClick[d] = 1;
      return d3.select(this).attr("opacity", 1).attr("fill", altpink).attr("stroke", purple);
    });
  }
  chr = markerchr[maxLod_marker];
  index = data.markerindex[chr][maxLod_marker];
  pos = data.lod[chr].pos[index];
  title = "(chr " + chr + ", " + (onedig(pos)) + " cM)";
  d3.selectAll("text#random_pxgtitle_marker").text(maxLod_marker);
  d3.selectAll("text#random_pxgtitle_position").text(title);
  plotPXG(maxLod_marker);
  return d3.select("circle#random_circ" + chr + "_" + index).attr("opacity", 1).attr("fill", altpink).attr("stroke", purple);
};

d3.json("data_random.json", draw);