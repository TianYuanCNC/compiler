#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <stack>
#include <unordered_map>

using namespace std;


typedef struct command{
	int index;
	int step;
	string type;
	string expression1;
	string expression2;
	string expression3;
	string expression4;
}COMMAND;

string StripZero(string s){
	int si = s.size();
	while(s[si-1] == '0')
		si--;
	if(s[si-1] == '.')
		si--;
	s.resize(si);
	return s;
}

string RemoveSpace(string s){
	int i, start=0;
	string result;
	for(i=start;i<s.size();i++){
		if(s[i] != ' ' && s[i]!='\n' && s[i]!='\r')
			result.push_back(s[i]);
	}
	return result;
}

class compiler{
private:
	string input_path, output_path, variable_path;
	vector<string> readedlines;
	vector<string> result;
	unordered_map<string, int> prior;
	vector<float> variables;
	vector<COMMAND> mycommand; //token

	vector<string> error_information;

	bool ExpressionToVector(string s, vector<string> &result);
	bool PostOrderExpression(vector<string> &result, vector<string> &post);
	bool CalculatePostOrderExpresion(vector<string> post, float &result);
	bool AnalizeExpression(string s, float &res);
	bool readinvariable();
	bool readinprogram();
	int FindIndex(int index);
	bool AnalyzeCommand(string s);
	bool GenerateOutput();
	void GenerateLogFile();

	bool CalculateExpression(string s, float &res);
public:
	compiler(string inputpath , string outputpath, string variablepath);
		
	void DumpVariables();
	void DumpCommand();
	bool compile();

};


compiler::compiler(string inputpath = "", string outputpath = "", string variablepath = ""){
	int i;
	input_path = inputpath;
	output_path = outputpath;
	variable_path = variablepath;
	prior.insert(pair<string, int>("eq", 0));
	prior.insert(pair<string, int>("lt", 0));
	prior.insert(pair<string, int>("le", 0));
	prior.insert(pair<string, int>("gt", 0));
	prior.insert(pair<string, int>("ge", 0));
	prior.insert(pair<string, int>("+", 1));
	prior.insert(pair<string, int>("-", 1));
	prior.insert(pair<string, int>("or", 1));
	prior.insert(pair<string, int>("xor", 1));
	prior.insert(pair<string, int>("*", 2));
	prior.insert(pair<string, int>("/", 2));
	prior.insert(pair<string, int>("and", 2));
	prior.insert(pair<string, int>("sin", 3));
	prior.insert(pair<string, int>("cos", 3));
	prior.insert(pair<string, int>("tan", 3));
	prior.insert(pair<string, int>("atan", 3));
	prior.insert(pair<string, int>("sqrt", 3));
	variables.resize(2002);
}

int compiler::FindIndex(int index){
	int i;
	for(i = 0; i<mycommand.size(); i++){
		if(mycommand[i].index == index)
			return i;
	}
	return -1;
}


bool compiler::ExpressionToVector(string s, vector<string> &result){
	string target = RemoveSpace(s);
	string temp;
	int si = target.size();
	int i;

	for(i=0;i<si;i++){
		if(target[i] == '#'){
			int j = i+1;
			while(j<si && (target[j] <= '9' && target[j] >= '0'))
				j++;
			if(j-i <= 1)
				return false;
			temp = target.substr(i+1, j-i-1);
			if(stoi(temp)<=0 || stoi(temp)>2001){
				string error;
				error = "Error (ExpressionToVector): #" + temp + " Invalid Line Index";
				error_information.push_back(error);
				return false;
			}
			result.push_back(to_string(variables[stoi(temp)]));
			temp.resize(0);
			i = j-1;
		}
		else if(target[i]>='0' && target[i]<='9'){
			int j = i;
			while(j<si && ((target[j] <= '9' && target[j] >= '0') || target[j] =='.'))
				j++;
			temp = target.substr(i, j-i);
			if(temp.back() == '.' || count(temp.begin(), temp.end(), '.') > 1){
				string error;
				error = "Error (ExpressionToVector): " + temp + " Invalid Constant Number";
				error_information.push_back(error);
				return false;
			}
			try{
				stof(temp);
			} 
			catch (exception &e){
				string error;
				error = "Error (ExpressionToVector): " + temp + " Invalid Constant Number";
				error_information.push_back(error);
				return false;
			}
			result.push_back(temp);
			temp.resize(0);
			i=j-1;
		}
		else if((target[i] == '-' || target[i] == '+') && (result.empty() || result.back() == "(" || result.back() == "[")){
			int j = i+1;
			if(target[j] == '#'){
				j++;
				while(j<si && (target[j] <= '9' && target[j] >= '0'))
					j++;
				if(j-i <= 2){
					string error;
					error = "Error (ExpressionToVector): Cannot Parse Variable";
					error_information.push_back(error);
					return false;
				}
				temp = target.substr(i+2, j-i-2);
				if(stoi(temp)<=0 || stoi(temp)>2001){
					string error;
					error = "Error (ExpressionToVector): #" + temp + " Invalid Variable Index";
					error_information.push_back(error);
					return false;
				}
				if(target[i] == '-')
					result.push_back(to_string(-variables[stoi(temp)]));
				else
					result.push_back(to_string(variables[stoi(temp)]));
				temp.resize(0);
				i = j-1;

			} else {
				while(j<si && ((target[j] <= '9' && target[j] >= '0') || target[j] =='.'))
					j++;
				temp = target.substr(i, j-i);
				if(temp.back() == '.' || count(temp.begin(), temp.end(), '.') > 1){
					string error;
					error = "Error (ExpressionToVector): " + temp + " Invalid Constant Number";
					error_information.push_back(error);
					return false;
				}
				try{
					stof(temp);
				} 
				catch (exception &e){
					string error;
					error = "Error (ExpressionToVector): #" + temp + " Invalid Constant Number";
					error_information.push_back(error);
					return false;
				}
				result.push_back(temp);
				temp.resize(0);
				i=j-1;
			}
		}
		else if(target[i] == '(' || target[i] == ')'){
			temp.push_back(target[i]);
			result.push_back(temp);
			temp.resize(0);
		}
		else if (target[i] == '+' || target[i] == '-' || target[i] == '*' || target[i] == '/'){
			temp.push_back(target[i]);
			result.push_back(temp);
			temp.resize(0);
		}
		else if(i+2<si && target.substr(i,3) == "SIN"){
			result.push_back("sin");
			i+=2;
		}
		else if(i+2<si && target.substr(i,3) == "COS"){
			result.push_back("cos");
			i+=2;
		}
		else if(i+2<si && target.substr(i,3) == "TAN"){
			result.push_back("tan");
			i+=2;
		}
		else if(i+3<si && target.substr(i,4) == "ATAN"){
			result.push_back("atan");
			i+=3;
		}
		else if(i+2<si && target.substr(i,3) == "AND"){
			result.push_back("and");
			i+=2;
		}
		else if(i+1<si && target.substr(i,2) == "OR"){
			result.push_back("or");
			i+=1;
		}
		else if(i+2<si && target.substr(i,3) == "XOR"){
			result.push_back("xor");
			i+=2;
		}
		else if(i+1<si && target.substr(i,2) == "EQ"){
			result.push_back("eq");
			i+=1;
		}
		else if(i+1<si && target.substr(i,2) == "GE"){
			result.push_back("ge");
			i+=1;
		}
		else if(i+1<si && target.substr(i,2) == "GT"){
			result.push_back("gt");
			i+=1;
		}
		else if(i+1<si && target.substr(i,2) == "LT"){
			result.push_back("lt");
			i+=1;
		}
		else if(i+1<si && target.substr(i,2) == "LE"){
			result.push_back("le");
			i+=1;
		}
		else if(i+3<si && target.substr(i,4) == "SQRT"){
			result.push_back("sqrt");
			i+=3;
		}
		else{
			string error;
			error = "Error (ExpressionToVector): Unknown Character Found";
			error_information.push_back(error);
			return false;
		}
	}
	return true;

}


bool compiler::PostOrderExpression(vector<string> &result, vector<string> &post){
	int i;
	stack<string> ope;

	for(i=0;i<result.size();i++){
		if(prior.find(result[i]) == prior.end() && result[i]!="(" && result[i]!=")"){
			post.push_back(result[i]);
		}
		else if(result[i] == "("){
			ope.push(result[i]);
		} 
		else if(result[i] == ")"){
			while(!ope.empty() &&ope.top() != "("){
				post.push_back(ope.top());
				ope.pop();
			}
			if(ope.empty()){
				string error;
				error = "Error (PostOrderExpression): Unmatched Paren Found";
				error_information.push_back(error);
				return false;
			} else {
				ope.pop();
			}
		}
		else {
			if(ope.empty() || ope.top() == "(" || prior.find(result[i])->second > prior.find(ope.top())->second){
				ope.push(result[i]);
			} else {
				while(!ope.empty() && ope.top() != "(" && prior.find(result[i])->second <= prior.find(ope.top())->second){
					post.push_back(ope.top());
					ope.pop();
				}
				ope.push(result[i]);
			}
		}
	}
	while(!ope.empty()){
		if(ope.top() == "("){
			string error;
			error = "Error (PostOrderExpression): Unmatched Paren Found";
			error_information.push_back(error);
			return false;
		} else {
			post.push_back(ope.top());
			ope.pop();
		}
	}
	return true;
}


bool compiler::CalculatePostOrderExpresion(vector<string> post, float &result){
	int i;
	stack<float> num;
	float num1, num2;
	for(i = 0; i< post.size();i++){
		if(post[i] == "+"){
			if(num.size() < 2)
				return false;
			num2 = num.top();
			num.pop();
			num1 = num.top();
			num.pop();
			num.push(num1+num2);
		}
		else if(post[i] == "-"){
			if(num.size() < 2)
					return false;
			num2 = num.top();
			num.pop();
			num1 = num.top();
			num.pop();
			num.push(num1-num2);
		}
		else if(post[i] == "*"){
			if(num.size() < 2)
				return false;
			num2 = num.top();
			num.pop();
			num1 = num.top();
			num.pop();
			num.push(num1*num2);
		}
		else if(post[i] == "/"){
			if(num.size() < 2)
				return false;
			num2 = num.top();
			num.pop();
			num1 = num.top();
			num.pop();
			num.push(num1/num2);
		}
		else if(post[i] == "and"){
			if(num.size() < 2)
				return false;
			num2 = num.top();
			num.pop();
			num1 = num.top();
			num.pop();
			num.push((float)((int)num1&&(int)num2));
		}
		else if(post[i] == "or"){
			if(num.size() < 2)
				return false;
			num2 = num.top();
			num.pop();
			num1 = num.top();
			num.pop();
			num.push((float)((int)num1||(int)num2));
		}
		else if(post[i] == "xor"){
			if(num.size() < 2)
					return false;
			num2 = num.top();
			num.pop();
			num1 = num.top();
			num.pop();
			num.push((float)((int)num1!=(int)num2));
		}
		else if(post[i] == "sin"){
			if(num.size() < 1)
				return false;
			num1 = num.top();
			num.pop();
			num.push((float)sin(num1*3.14159265/180));
		}
		else if(post[i] == "cos"){
			if(num.size() < 1)
				return false;
			num1 = num.top();
			num.pop();
			num.push((float)cos(num1*3.14159265/180));
		}
		else if(post[i] == "tan"){
			if(num.size() < 1)
				return false;
			num1 = num.top();
			num.pop();
			num.push((float)tan(num1*3.14159265/180));
		}
		else if(post[i] == "atan"){
			if(num.size() < 1)
				return false;
			num1 = num.top();
			num.pop();
			num.push((float)atan(num1)*180/3.14159265);
		}
		else if(post[i] == "eq"){
			if(num.size() < 2)
				return false;
			num2 = num.top();
			num.pop();
			num1 = num.top();
			num.pop();
			num.push((float)(num1 == num2));
		}
		else if(post[i] == "elt"){
			if(num.size() < 2)
				return false;
			num2 = num.top();
			num.pop();
			num1 = num.top();
			num.pop();
			num.push((float)(num1 < num2));
		}
		else if(post[i] == "le"){
			if(num.size() < 2)
				return false;
			num2 = num.top();
			num.pop();
			num1 = num.top();
			num.pop();
			num.push((float)(num1 <= num2));
		}
		else if(post[i] == "ge"){
			if(num.size() < 2)
				return false;
			num2 = num.top();
			num.pop();
			num1 = num.top();
			num.pop();
			num.push((float)(num1 >= num2));
		}
		else if(post[i] == "gt"){
			if(num.size() < 2)
				return false;
			num2 = num.top();
			num.pop();
			num1 = num.top();
			num.pop();
			num.push((float)(num1 > num2));
		}
		else if(post[i] == "sqrt"){
			if(num.size() < 1)
				return false;
			num1 = num.top();
			num.pop();
			num.push((float)(sqrt(num1)));
		}
		else{
			num.push(stof(post[i]));
		}
	}
	if(num.size() != 1){
		string error;
		error = "Error (CalculatePostOrderExpresion): Expression Cannot Be Calculated";
		error_information.push_back(error);
		return false;
	}
	result=num.top();
	return true;
}


bool compiler::AnalizeExpression(string s, float &res){
	stack<char> mystack;
	int i;
	string res_temp;

	for(i=0;i<s.size();i++){
		if(s[i] != ']'){
			mystack.push(s[i]);
		} else {
			string temp;
			float temp_res;
			int j;
			while(!mystack.empty() && mystack.top()!='['){
				temp.push_back(mystack.top());
				mystack.pop();
			}
			if(mystack.empty()){
				string error;
				error = "Error (AnalizeExpression): Unmatched Paren Found";
				error_information.push_back(error);
				return false;
			}
			mystack.pop();
			reverse(temp.begin(), temp.end());
			if(!CalculateExpression(temp, temp_res)){
				string error;
				error = "Error (AnalizeExpression): Cannot Calculate Subexpression " + temp;
				error_information.push_back(error);
				return false;
			}
			temp.resize(0);
			if(!mystack.empty() && mystack.top() == '#')
				temp = to_string((int)temp_res);
			else
				temp = to_string(temp_res);	

			for(j=0;j<temp.size();j++)
				mystack.push(temp[j]);
		}
	}
	while(!mystack.empty()){
		res_temp.push_back(mystack.top());
		mystack.pop();
	}
	reverse(res_temp.begin(), res_temp.end());
	if(!CalculateExpression(res_temp, res)){
		string error;
		error = "Error (AnalizeExpression): Cannot Calculate Expression" + s;
		error_information.push_back(error);
		return false;
	}
	return true;
}


bool compiler::AnalyzeCommand(string s){
	string target = RemoveSpace(s);
	string temp;
	int si = target.size();
	int i = 0;
	int step = 0;
	COMMAND new_command;

	if(target.size() == 0)
		return true;

	while(step < 3){
		switch(step){

			case 0:
			if(s[i] == 'N'){
				int j = i+1;
				while(j<si && (target[j] <= '9' && target[j] >= '0'))
					j++;
				if(j-i <= 1){
					string error;
					error = "Error (AnalyzeCommand): Invalid Line Index";
					error_information.push_back(error);
					return false;
				}
				temp = target.substr(i+1, j-i-1);
				if(stoi(temp) < 0){
					string error;
					error = "Error (AnalyzeCommand): Invalid Line Index" + temp;
					error_information.push_back(error);
					return false;
				}
				new_command.index = stoi(temp);
				i = j;
			} else {
				new_command.index = -1;
			}
			step++;
			break;


			case 1:
			if(i+1<si && target.substr(i,2) == "IF"){
				new_command.type = "if";
				i+=2;
			}
			else if(i+2<si && target.substr(i,3) == "G00"){
				new_command.type = "g00";
				i+=3;
			}
			else if(i+2<si && target.substr(i,3) == "G01"){
				new_command.type = "g01";
				i+=3;
			}
			else if(i+2<si && target.substr(i,3) == "G02"){
				new_command.type = "g02";
				i+=3;
			}
			else if(i+2<si && target.substr(i,3) == "G03"){
				new_command.type = "g03";
				i+=3;
			}
			else if(i+2<si && target.substr(i,3) == "G40"){
				new_command.type = "g40";
				i+=3;
			}
			else if(i+2<si && target.substr(i,3) == "G92"){
				new_command.type = "g92";
				i+=3;
			}
			else if(i+2<si && target.substr(i,3) == "G41"){
				new_command.type = "g41";
				i+=3;
			}
			else if(i+2<si && target.substr(i,3) == "G42"){
				new_command.type = "g42";
				i+=3;
			}
			else if(i+2<si && target.substr(i,3) == "M07"){
				new_command.type = "m07";
				i+=3;
			}
			else if(i+2<si && target.substr(i,3) == "M08"){
				new_command.type = "m08";
				i+=3;
			}
			else if(i+2<si && target.substr(i,3) == "M99"){
				new_command.type = "m99";
				i+=3;
			}
			else if(i+2<si && target.substr(i,3) == "END"){
				new_command.type = "end";
				i+=3;
			}
			else if(i+3<si && target.substr(i,4) == "GOTO"){
				new_command.type = "goto";
				i+=4;
			}
			else if(i+4<si && target.substr(i,5) == "WHILE"){
				new_command.type = "while";
				i+=5;
			}
			else if(target[i] == '#' && target.find_first_of('=', i) != string::npos){
				new_command.type = "assign";
				i++;
			}
			else{
				string error;
				error = "Error (AnalyzeCommand): Unknown Command Found";
				error_information.push_back(error);
				return false;
			}
			step++;
			break;


			case 2:
			if(new_command.type == "g00"){
				int px = target.find_first_of('X', i);
				int py = target.find_first_of('Y', i);
				if(px == string::npos || py == string::npos || py<px){
					string error;
					error = "Error (AnalyzeCommand): Too Few Parameters for G00";
					error_information.push_back(error);
					return false;
				}
				new_command.expression1 = "[" + target.substr(px+1, py-px-1) + "]";
				new_command.expression2 = "[" + target.substr(py+1, si-py-1) + "]";
			}
			else if(new_command.type == "g01"){
				int px = target.find_first_of('X', i);
				int py = target.find_first_of('Y', i);
				if(px == string::npos || py == string::npos || py<px){
					string error;
					error = "Error (AnalyzeCommand): Too Few Parameters for G01";
					error_information.push_back(error);
					return false;
				}
				new_command.expression1 = "[" + target.substr(px+1, py-px-1) + "]";
				new_command.expression2 = "[" + target.substr(py+1, si-py-1) + "]";
			}
			else if(new_command.type == "g92"){
				int px = target.find_first_of('X', i);
				int py = target.find_first_of('Y', i);
				if(px == string::npos || py == string::npos || py<px){
					string error;
					error = "Error (AnalyzeCommand): Too Few Parameters for G92";
					error_information.push_back(error);
					return false;
				}
				new_command.expression1 = "[" + target.substr(px+1, py-px-1) + "]";
				new_command.expression2 = "[" + target.substr(py+1, si-py-1) + "]";
			}
			else if(new_command.type == "g02"){
				int px = target.find_first_of('X', i);
				int py = target.find_first_of('Y', i);
				int pi = target.find_first_of('I', i);
				int pj = target.find_first_of('J', i);
				int pr = target.find_first_of('R', i);
				if(px == string::npos || py == string::npos){
					string error;
					error = "Error (AnalyzeCommand): Too Few Parameters for G02";
					error_information.push_back(error);
					return false;
				}
				if(pr == string::npos &&(pi == string::npos || pj == string::npos)){
					string error;
					error = "Error (AnalyzeCommand): Too Few Parameters for G02";
					error_information.push_back(error);
					return false;
				}
				new_command.expression1 = "[" + target.substr(px+1, py-px-1) + "]";
				if(pr != string::npos){
					new_command.expression2 = "[" + target.substr(py+1, pr-py-1) + "]";
					new_command.expression3 = "[" + target.substr(pr+1, si-pr-1) + "]";
				} else {
					new_command.expression2 = "[" + target.substr(py+1, pi-py-1) + "]";
					new_command.expression3 = "[" + target.substr(pi+1, pj-pi-1) + "]";
					new_command.expression2 = "[" + target.substr(pj+1, si-pj-1) + "]";
				}
			}
			else if(new_command.type == "g03"){
				int px = target.find_first_of('X', i);
				int py = target.find_first_of('Y', i);
				int pi = target.find_first_of('I', i);
				int pj = target.find_first_of('J', i);
				int pr = target.find_first_of('R', i);
				if(px == string::npos || py == string::npos){
					string error;
					error = "Error (AnalyzeCommand): Too Few Parameters for G02";
					error_information.push_back(error);
					return false;
				}
				if(pr == string::npos &&(pi == string::npos || pj == string::npos)){
					string error;
					error = "Error (AnalyzeCommand): Too Few Parameters for G02";
					error_information.push_back(error);
					return false;
				}
				new_command.expression1 = "[" + target.substr(px+1, py-px-1) + "]";
				if(pr != string::npos){
					new_command.expression2 = "[" + target.substr(py+1, pr-py-1) +"]";
					new_command.expression3 = "[" + target.substr(pr+1, si-pr-1) + "]";
				} else {
					new_command.expression2 = "[" + target.substr(py+1, pi-py-1) + "]";
					new_command.expression3 = "[" + target.substr(pi+1, pj-pi-1) + "]";
					new_command.expression2 = "[" + target.substr(pj+1, si-pj-1) + "]";
				}
			}
			else if(new_command.type == "assign"){
				int equ_pos = target.find_first_of('=', 0);
				if(equ_pos == string::npos){
					string error;
					error = "Error (AnalyzeCommand): Invalid Assignment Command";
					error_information.push_back(error);
					return false;
				}
				new_command.expression1 = target.substr( i, equ_pos-i);
				new_command.expression2 = "[" + target.substr(equ_pos+1, si-equ_pos-1) + "]";
			}
			else if(new_command.type == "end"){
				new_command.expression1 = target.substr(i, si-i);
			}
			else if(new_command.type == "if"){
				int pos_left = i;
				int pos_excute, pos_right, pos_com;
				if((pos_excute = target.find("THEN"))!=string::npos){
					new_command.expression1 = "then";
				} else if((pos_excute = target.find("GOTO"))!=string::npos){
					new_command.expression1 = "goto";
				} else {
					string error;
					error = "Error (AnalyzeCommand): Cannot Find Condition Action";
					error_information.push_back(error);
					return false;
				}

				pos_right = pos_excute-1;
				if(target[pos_left] != '[' || target[pos_right] != ']' || pos_left >= pos_right){
					string error;
					error = "Error (AnalyzeCommand): Unmatched Paren Found";
					error_information.push_back(error);
					return false;
				}

				new_command.expression2 = "[" + target.substr(pos_left+1, pos_right-pos_left-1) + "]";
				if(new_command.expression1 == "goto")
					new_command.expression3 = "[" + target.substr(pos_excute+4, si-pos_excute-4) + "]";

			}
			else if(new_command.type == "while"){
				int pos_left = i;
				int pos_excute, pos_right, pos_com;
				if((pos_excute = target.find("DO"))==string::npos){
					string error;
					error = "Error (AnalyzeCommand): While Level not Specified";
					error_information.push_back(error);
					return false;
				}

				pos_right = pos_excute-1;
				if(target[pos_left] != '[' || target[pos_right] != ']' || pos_left >= pos_right){
					string error;
					error = "Error (AnalyzeCommand): Unmatched Paren Found";
					error_information.push_back(error);
					return false;
				}

				new_command.expression1 = "[" + target.substr(pos_left+1, pos_right-pos_left-1) + "]";
				new_command.expression2 = target.substr(pos_excute+2, si-pos_excute-2);

			}
			else if(new_command.type == "goto"){
				new_command.expression1 = target.substr(i, si-i);
			}
			else if(new_command.type == "g40" || new_command.type == "g41" || new_command.type == "g42" ||
					new_command.type == "m07" || new_command.type == "m08" || new_command.type == "m99"){
				;
			}
			else{
				string error;
				error = "Error (AnalyzeCommand): Unknown Command Found" + new_command.type;
				error_information.push_back(error);
				return false;
			}
			step++;
			break;

			default: break;
		}
	}
	mycommand.push_back(new_command);
	return true;
}

bool compiler::GenerateOutput(){
	int cur_line = 0;
	ofstream out;
	string temp;
	int i;

	out.open(output_path);
	if(!out.is_open()){
		cout << "Error: Cannot Open Output File\n";
		return false;
	}
	for(i=0;i<mycommand.size();i++){
		if(mycommand[i].type == "goto"){
			float pos;
			if(!AnalizeExpression(mycommand[i].expression1, pos)){
				string error;
				error = "Error (GenerateOutput): Cannot Calculate Line Index" + mycommand[i].expression1;
				error_information.push_back(error);
				return false;
			}
			i = FindIndex((int)pos)-1;
			if(i<0){
				string error;
				error = "Error (GenerateOutput): Invalid Line Index" + mycommand[i].expression1;
				error_information.push_back(error);
				return false;
			}
		}
		else if(mycommand[i].type == "g00"){
			float res;
			temp+="G00 X";
			if(!AnalizeExpression(mycommand[i].expression1, res)){
				string error;
				error = "Error (GenerateOutput): Invalid X Expression" + mycommand[i].expression1;
				error_information.push_back(error);
				return false;
			}
			temp+=StripZero(to_string(res));
			temp+=" Y";
			if(!AnalizeExpression(mycommand[i].expression2, res)){
				string error;
				error = "Error (GenerateOutput): Invalid Y Expression" + mycommand[i].expression2;
				error_information.push_back(error);
				return false;
			}
			temp+=StripZero(to_string(res));
			out << temp << "\n";
			temp.resize(0);
		}
		else if(mycommand[i].type == "g01"){
			float res;
			temp+="G01 X";
			if(!AnalizeExpression(mycommand[i].expression1, res)){
				string error;
				error = "Error (GenerateOutput): Invalid X Expression" + mycommand[i].expression1;
				error_information.push_back(error);
				return false;
			}
			temp+=StripZero(to_string(res));
			temp+=" Y";
			if(!AnalizeExpression(mycommand[i].expression2, res)){
				string error;
				error = "Error (GenerateOutput): Invalid Y Expression" + mycommand[i].expression2;
				error_information.push_back(error);
				return false;
			}
			temp+=StripZero(to_string(res));
			out << temp << "\n";
			temp.resize(0);
		}
		else if(mycommand[i].type == "g02"){
			float res;
			temp+="G02 X";
			if(!AnalizeExpression(mycommand[i].expression1, res)){
				string error;
				error = "Error (GenerateOutput): Invalid X Expression" + mycommand[i].expression1;
				error_information.push_back(error);
				return false;
			}
			temp+=StripZero(to_string(res));
			temp+=" Y";
			if(!AnalizeExpression(mycommand[i].expression2, res)){
				string error;
				error = "Error (GenerateOutput): Invalid Y Expression" + mycommand[i].expression2;
				error_information.push_back(error);
				return false;
			}
			temp+=StripZero(to_string(res));
			if(mycommand[i].expression4 == ""){
				temp+=" R";
				if(!AnalizeExpression(mycommand[i].expression3, res)){
					string error;
					error = "Error (GenerateOutput): Invalid R Expression" + mycommand[i].expression3;
					error_information.push_back(error);
					return false;
				}
				temp+=StripZero(to_string(res));
			} else {
				temp+=" I";
				if(!AnalizeExpression(mycommand[i].expression3, res)){
					string error;
					error = "Error (GenerateOutput): Invalid I Expression" + mycommand[i].expression3;
					error_information.push_back(error);
					return false;
				}
				temp+=StripZero(to_string(res));
				temp+=" J";
				if(!AnalizeExpression(mycommand[i].expression3, res)){
					string error;
					error = "Error (GenerateOutput): Invalid J Expression" + mycommand[i].expression4;
					error_information.push_back(error);
					return false;
				}
				temp+=StripZero(to_string(res));
			}
			out << temp << "\n";
			temp.resize(0);
		}
		else if(mycommand[i].type == "g03"){
			float res;
			temp+="G03 X";
			if(!AnalizeExpression(mycommand[i].expression1, res)){
				string error;
				error = "Error (GenerateOutput): Invalid X Expression" + mycommand[i].expression1;
				error_information.push_back(error);
				return false;
			}
			temp+=StripZero(to_string(res));
			temp+=" Y";
			if(!AnalizeExpression(mycommand[i].expression2, res)){
				string error;
				error = "Error (GenerateOutput): Invalid Y Expression" + mycommand[i].expression2;
				error_information.push_back(error);
				return false;
			}
			temp+=StripZero(to_string(res));
			if(mycommand[i].expression4 == ""){
				temp+=" R";
				if(!AnalizeExpression(mycommand[i].expression3, res)){
					string error;
					error = "Error (GenerateOutput): Invalid R Expression" + mycommand[i].expression3;
					error_information.push_back(error);
					return false;
				}
				temp+=StripZero(to_string(res));
			} else {
				temp+=" I";
				if(!AnalizeExpression(mycommand[i].expression3, res)){
					string error;
					error = "Error (GenerateOutput): Invalid I Expression" + mycommand[i].expression3;
					error_information.push_back(error);
					return false;
				}
				temp+=StripZero(to_string(res));
				temp+=" J";
				if(!AnalizeExpression(mycommand[i].expression3, res)){
					string error;
					error = "Error (GenerateOutput): Invalid J Expression" + mycommand[i].expression4;
					error_information.push_back(error);
					return false;
				}
				temp+=StripZero(to_string(res));
			}
			out << temp << "\n";
			temp.resize(0);
		}
		else if(mycommand[i].type == "g40"){
			out << "G40\n";
		}
		else if(mycommand[i].type == "g41"){
			out << "G41\n";
		}
		else if(mycommand[i].type == "g42"){
			out << "G42\n";
		}
		else if(mycommand[i].type == "g92"){
			float res;
			temp+="G92 X";
			if(!AnalizeExpression(mycommand[i].expression1, res)){
				string error;
				error = "Error (GenerateOutput): Invalid X Expression" + mycommand[i].expression1;
				error_information.push_back(error);
				return false;
			}
			temp+=StripZero(to_string(res));
			temp+=" Y";
			if(!AnalizeExpression(mycommand[i].expression2, res)){
				string error;
				error = "Error (GenerateOutput): Invalid Y Expression" + mycommand[i].expression2;
				error_information.push_back(error);
				return false;
			}
			temp+=StripZero(to_string(res));
			out << temp << "\n";
			temp.resize(0);
		}
		else if(mycommand[i].type == "m07"){
			out << "M07\n";
		}
		else if(mycommand[i].type == "m08"){
			out << "M08\n";
		}
		else if(mycommand[i].type == "m99"){
			out << "M99\n";
		}
		else if(mycommand[i].type == "assign"){
			int var_index;
			float res;
			try{
				var_index = stoi(mycommand[i].expression1);
			} catch(exception &e){
				string error;
				error = "Error (GenerateOutput): Invalid Assignment Target Variable" + mycommand[i].expression1;
				error_information.push_back(error);
				return false;
			}
			if(var_index<=0 || var_index>2001){
				string error;
				error = "Error (GenerateOutput): Invalid Assignment Target Variable" + mycommand[i].expression1;
				error_information.push_back(error);
				return false;
			}
			if(!AnalizeExpression(mycommand[i].expression2, res)){
				string error;
				error = "Error (GenerateOutput): Invalid Assignment Calculate Expression" + mycommand[i].expression2;
				error_information.push_back(error);
				return false;
			}
			variables[var_index] = res; 
		}
		else if(mycommand[i].type == "if"){
			float cond;
			if(!AnalizeExpression(mycommand[i].expression2, cond)){
				string error;
				error = "Error (GenerateOutput): Cannot Calculate Condition Expression" + mycommand[i].expression2;
				error_information.push_back(error);
				return false;
			}

			if((int)cond != 0){
				if(mycommand[i].expression1 == "goto"){
					float pos;
					if(!AnalizeExpression(mycommand[i].expression3, pos)){
						cout << "Error: Cannot Calculate Target\n";
						return false;
					}
					i = FindIndex((int)pos)-1;
					if(i<0){
						cout << "Error: Goto Unknown Line Index";
						return false;
					}
				}
				else if(mycommand[i].expression1 == "then"){
					continue;
				}
				else{
					cout << "Error: Unknown Operation\n";
					return false;
				}
			} else {
				if(mycommand[i].expression1 == "goto"){
					continue;
				}
				else if(mycommand[i].expression1 == "then"){
					i++;
				}
				else{
					cout << "Error: Unknown Operation\n";
					return false;
				}

			}
		}
		else if(mycommand[i].type == "while"){
			float cond;
			if(!AnalizeExpression(mycommand[i].expression1, cond)){
				string error;
				error = "Error (GenerateOutput): Cannot Calculate Condition Expression" + mycommand[i].expression1;
				error_information.push_back(error);
				return false;
			}

			if((int)cond == 0){
				int k;
				for(k = i+1; k<mycommand.size();k++){
					if(mycommand[k].type == "end" && mycommand[k].expression1 == mycommand[i].expression2)
						break;
				}
				if(k == mycommand.size()){
					string error;
					error = "Error (GenerateOutput): While Cannot Find Ending Statement";
					error_information.push_back(error);
					return false;
				}
				i = k;
			}
		}
		else if(mycommand[i].type == "end"){
			int k;
			for(k=i-1;k>=0;k--){
				if(mycommand[k].type == "while" && mycommand[k].expression2 == mycommand[i].expression1)
					break;
			}
			if(k < 0){
				string error;
				error = "Error (GenerateOutput): While Cannot Find Ending Statement";
				error_information.push_back(error);
				return false;
			}
			i = k-1;
		}
		else {
			string error;
			error = "Error (GenerateOutput): Unknown Command Found";
			error_information.push_back(error);
			return false;
		}
	}

	out.close();
	return true;

}


bool compiler::CalculateExpression(string s, float &res){
	vector<string> result;
	vector<string> post;
	int i;
	if(!ExpressionToVector(s, result))
		return false;
	if(!PostOrderExpression(result, post))
		return false;

	if(!CalculatePostOrderExpresion(post, res))
		return false;
	return true;
}

void compiler::DumpVariables(){
	int i;
	cout << "#2001:" << variables[0] << "\n";
	for(i=1;i<variables.size();i++){
		cout << "#" << i << ": " << variables[i] << "\n";
	}
}


void compiler::DumpCommand(){
	int i;
	for(i=0;i<mycommand.size();i++){
		cout << "Line: " << mycommand[i].index << "\n"
			 << "Type: " << mycommand[i].type << "\n"
			 << "E1: " << mycommand[i].expression1 << "\n"
			 << "E2: " << mycommand[i].expression2 << "\n"
			 << "E3: " << mycommand[i].expression3 << "\n"
			 << "E4: " << mycommand[i].expression4 << "\n";
	}
}

bool compiler::readinprogram(){
	string line;
	ifstream myfile;
	int line_count = 1;

	myfile.open(input_path);
	if (!myfile.is_open()){
		string error;
		error = "Error (readinprogram): Cannot Open Source File "+input_path;
		error_information.push_back(error);
		return false;
	}

	while(getline (myfile,line)){
		if(!AnalyzeCommand(line)){
			string error;
			error = "Error (readinprogram): Line "+line_count;
			error_information.push_back(error);
			return false;
		}			
		line_count++;
	}			
	return true;
}
bool compiler::readinvariable(){
	string line;
	ifstream myfile;
	int line_count = 1;

	myfile.open(variable_path);
	if (!myfile.is_open()){
		string error;
		error = "Error (readinvariable): Cannot Open Variable File "+variable_path;
		error_information.push_back(error);
		return false;
	}
	while(getline (myfile,line)){
		size_t equal;
		int var_index; float var_value;
		equal = line.find_first_of('=');
		if(equal == string::npos){
			string error;
			error = "Error (readinvariable): Invalid Variable Defination "+line_count;
			error_information.push_back(error);
			return false;
		}
		try {
			string temp = line.substr(equal+1, line.size()-equal-1);
			var_index = stoi(line.substr(1, equal-1));
			if(var_index <= 0 || (var_index > 33 && var_index!=2001)){
				string error;
				error = "Error (readinvariable): Invalid Variable Index "+line_count;
				error_information.push_back(error);
				return false;
			}

			if(count(temp.begin(), temp.end(), '.') <= 1){
				variables[var_index] = stof(temp);
			}
			else{
				string error;
				error = "Error (readinvariable): Invalid Constant Number "+line_count;
				error_information.push_back(error);
				return false;
			}
		}
		catch(exception &e){
			string error;
			error = "Error (readinvariable): Invalid Variable Defination "+line_count;
			error_information.push_back(error);
			return false;
		}
		line_count++;
	}			
	return true;
}

bool compiler::compile(){
	float res=0;
	int i;
	if(!readinvariable()){
		error_information.push_back("Compile Failed!!!");
		GenerateLogFile();
		return false;
	}
	if(!readinprogram()){
		error_information.push_back("Compile Failed!!!");
		GenerateLogFile();
		return false;
	}
	if(!GenerateOutput()){
		error_information.push_back("Compile Failed!!!");
		GenerateLogFile();
		return false;
	}
	error_information.push_back("Compile Succeed!!!");
	GenerateLogFile();
	return true;
}

void compiler::GenerateLogFile(){
	ofstream err;
	int i;

	err.open("compile-log");
	if(!err.is_open()){
		cout << "Error: Cannot Open Error File, Ignored\n";
		return;
	}
	for(i=0;i<error_information.size();i++){
		err << error_information[i] << "\n";
	}
	err.close();

}

int main(int argc, char *argv[]){
	float res=0;
	int i;
	compiler test(argv[1], argv[2], argv[3]);
	if(!test.compile()){
		cout << "Compile Failed! Please Check Log File!!!\n";
	} else {
		cout << "Compile Succeed!!!\n";
	}
	return 0;
}
