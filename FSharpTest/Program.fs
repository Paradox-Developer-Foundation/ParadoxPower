module Program

open ParadoxPower.CSharp
open ParadoxPower.Parser


let text = """
state={
        	id=1
        	name="STATE_1" # Corsica
        	manpower = 322900
        	
        	state_category = town

        	history={
        		owner = FRA
        		victory_points = { 3838 1 }
        		buildings = {
        			infrastructure = 2
        			industrial_complex = 1
        			air_base = 1
        			3838 = {
        				naval_base = 3
        			}
        		}
        		add_core_of = COR
        		add_core_of = FRA
        	}

        	provinces={
        		3838 9851 11804 
        	}

        	local_supplies=0.0 
        }

"""

let text1 = """
state={
        	id=2
        	name="STATE_21" # Corsica1
        	manpower = 3229001
        	
        	state_category = town1

        	history={
        		owner = FRA1
        		victory_points = { 3838 1 }
        		buildings = {
        			infrastructure = 1
        			industrial_complex = 11
        			air_base = 11
        			3838 = {
        				naval_base = 31
        			}
        		}
        		add_core_of = COR1
        		add_core_of = FRA1
        	}

        	provinces={
        		3838 98511 118041 
        	}

        	local_supplies=0.0 
        

"""

// let node = Parsers.ProcessStatements("123", "123",
//         Parsers.ParseScriptFile("123.txt", text1).GetResult())

let mess = Parsers.ParseScriptFile("123.txt", text1).GetError()
printf $"%s{mess.ErrorMessage}"