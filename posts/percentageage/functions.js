<script>
function convert_age()
{
        age = document.getElementById("age").value;
        document.getElementById("iron").innerHTML = create_percentage(age, 1200+2023);
		document.getElementById("corn").innerHTML = create_percentage(age, 8700); // https://www.nsf.gov/news/news_summ.jsp?cntn_id=114445
		document.getElementById("lightbulb").innerHTML = create_percentage(age,2023-1879); 
		document.getElementById("credit").innerHTML = create_percentage(age,2023-1974);
		document.getElementById("segregation").innerHTML = create_percentage(age,2023-1964);
		document.getElementById("flight").innerHTML = create_percentage(age,2023-1903);
		document.getElementById("email").innerHTML = create_percentage(age,2023-1971);
		document.getElementById("printing").innerHTML = create_percentage(age,2023-1440);
		document.getElementById("platetectonics").innerHTML = create_percentage(age,2023-1965);
		document.getElementById("womenvote").innerHTML = create_percentage(age,2023-1920);
		document.getElementById("dna").innerHTML = create_percentage(age,2023-1953);


}

function create_percentage(age, years_elapsed)
{
	var percentage = 100*age / years_elapsed;
	return percentage.toFixed(1);
}

document.addEventListener('readystatechange', () => {    
  if (document.readyState == 'complete') convert_age();
});
    </script>