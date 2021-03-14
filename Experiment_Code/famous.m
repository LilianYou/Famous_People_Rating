function   DATA = famous()       
  
%Start by removing anything from the memory
clear all; close all;      
%Priority(MaxPriority); %Set Max Priority
%Set DebugLevel to 3
Screen('Preference', 'VisualDebuglevel', 3);
Screen('Preference','SkipSyncTests', 1);
Screen('Preference', 'SuppressAllWarnings');
dbstop if error
FlushEvents; 
%set debugmode to 1 for testing (should be 0 for actual experiment)  
debugmode=0;
%% EXPERIMENT AND CONDITION INFORMATION
expName = 'famous';

%% GET SUBJECT INFORMATION
if debugmode == 1, subjectName = 'TEST'; subjectAge = 'TEST'; subjectGender = 'TEST';
else
    subjectName = inputdlg('Your name?');
    subjectGender = inputdlg('Gender?');
    subjectUSyears = inputdlg('How many years have you been staying in U.S.?');
end
  
%% CREATE LOGFILE FOR WRITING DATA  v
logfile=strcat(subjectName,'_',expName,'_',strrep(strrep(datestr(now),' ','_'),':','-'),'.log');
if debugmode == 0,logfile=logfile{1};end
fid=fopen(logfile,'w'); %obtain the integer file identifier for logfile

%writes column titles in text document
fprintf(fid,'trial\tstimulus\tstimNum\tkey\n'); 
   

%% SET UP DATA STRUCTURE
% This structure should contain every detail necessary to reconstruct the
% sequence and timing of the experiment, and the responses. j
% subject details   
DATA.subject = subjectName;
%when was the script started?
DATA.when_start = datestr(now);
%location of input file containing list of stimuli
%DATA.inputList = [pwd,'/inputlist_stimuli.txt'];
DATA.inputList = [pwd,'/inputlist_stimuli_var1.txt'];
% read from input file the list of stimuli 
%[DATA.stimulus, DATA.number, DATA.pair, DATA.firstname, DATA.lastname, DATA.format ] = textread(DATA.inputList,'%s%d%d%s%s%s');
[DATA.stimulus, DATA.number, DATA.pair, DATA.firstname, DATA.middlename, DATA.lastname, DATA.format ] = textread(DATA.inputList,'%s%d%d%s%s%s%s');

DATA.examples = {'Margaret_Thatcher'};
DATA.examplesName  = {'Margaret   Thatcher'};

% % the total number of pictures to be presented determine the number of trials
DATA.n_trials = length(DATA.stimulus);  
  
% initialize random number generator
rand('seed', sum(100*clock)); %seeds the randon number generator with the clock time
%create a randomized vector specifying the orderof stimuli
DATA.random_order = randperm(DATA.n_trials);  % RANDPERM (n) is a random permutation of the integers from 1 to n

%create data structure for experiment, used in storing data for each trial
DATA.exp_data = zeros(DATA.n_trials,20); %initialize data array with 3 columns
DATA.exp_data_columns = {'trial', 'stimulus', 'stimulusNo', 'pairNo','FristName','LastName','key'};
%% PTB3 CONFIGURATION
% display parameters
% Screen('CloseAll');
% screens=Screen('Screens');%number of screens opened; should be 0
% D.screenNumber=max(screens);   j h v 
D.rs=0;D.cs=0;%initialize width and height of stimulus; will differ for each stimulus
D.rect=[0 0 D.cs D.rs];%stimulus display
% pixelSizes=Screen('PixelSizes',D.screenNumber);if max(pixelSizes)<16,fprintf('Sorry, I need a screen that supports 16- or 32-bit pixelSize.\n');return;end
if debugmode == 1 
    screenrect=[  0 0 1920 1080];
    %screenrect=[0 0 400 250];
    else screenrect=[];
end
[D.window D.screenRect]=Screen('OpenWindow', 0, [], screenrect);
%D.frameRate=Screen('FrameRate',D.screenNumber) 
%WaitSecs(0.01);GetSecs;
D.screensize_r = D.screenRect(4);
D.screensize_c = D.screenRect(3);
D.rect_center = D.rect + [D.screensize_c/2-D.cs/2 D.screensize_r/2-D.rs/2 D.screensize_c/2-D.cs/2 D.screensize_r/2-D.rs/2];

% text setup
Screen('TextSize', D.window, round(D.screenRect(3)*.019)); %text size  
D.textcolor=[0 0 0];
D.vspace=D.screenRect(3)*.03; 
D.hstart=10;
D.vstart=10;
%% START DISPLAY        
HideCursor;
% put instruction text
Screen('DrawText',D.window,'Hello - Welcome to the experiment.',D.hstart,D.vstart,D.textcolor);  
Screen('DrawText',D.window,'Press <RightArrow> to continue and <esc> to abort.',D.hstart,D.vstart+D.vspace,D.textcolor); 
Screen('Flip',D.window);

KbName('UnifyKeyNames');

KEY.space=KbName('space');
KEY.esc=KbName('escape');
KEY.same = KbName('LeftArrow');  % key for 'same' response
KEY.different = KbName('RightArrow');  % key for 'different' responses

reply=zeros(1,256);
while ~sum(reply([KEY.different KEY.esc])),   
    WaitSecs(0.01);
    [keyIsDown, secs, reply] = KbCheck;
end
if reply(KEY.esc)
    fprintf(fid,'...aborted\n');
    sca;
    clear all;
    return;
end
%% Instructions
Screen('DrawText',D.window,'During this experiment we will be showing you different pictures of well-known people. ',D.hstart,D.vstart,D.textcolor);  
Screen('DrawText',D.window,'When the picture is shown you will answer 7 questions by typing your answer on a line. ',D.hstart,D.vstart+1*D.vspace,D.textcolor);
Screen('DrawText',D.window,'You will need to answer specific questions to indicate what you know about each person.',D.hstart,D.vstart+2*D.vspace,D.textcolor);
Screen('DrawText',D.window,'We will ask you what the person is known for, approximately what time period is most',D.hstart,D.vstart+3*D.vspace,D.textcolor);
Screen('DrawText',D.window,'associated with this person, and what place this person is most associated with.',D.hstart,D.vstart+4*D.vspace,D.textcolor);

Screen('DrawText',D.window,'Then we will ask you questions based on your own social distance,temporal distance, ',D.hstart,D.vstart+6*D.vspace,D.textcolor);
Screen('DrawText',D.window,'and spatial distance with that famous person. You will answer these questions ',D.hstart,D.vstart+7*D.vspace,D.textcolor);
Screen('DrawText',D.window,'accordingly based on a scale from 1 to 7, with 1 being close and 7 being far.  ',D.hstart,D.vstart+8*D.vspace,D.textcolor);

Screen('DrawText',D.window,'If you dont have any idea of who they are or their time and place, please write',D.hstart,D.vstart+10*D.vspace,D.textcolor);
Screen('DrawText',D.window,'   "I dont know"    for these lines or "0" for rating. For all of these questions, ',D.hstart,D.vstart+11*D.vspace,D.textcolor);
Screen('DrawText',D.window,'put down your first impressions and dont think too long about any particular questions.',D.hstart,D.vstart+12*D.vspace,D.textcolor);

Screen('DrawText',D.window,'Press <space> to start and <esc> to abort.',D.hstart,D.vstart+15 *D.vspace,D.textcolor);
Screen('Flip',D.window);
   
% while KbCheck
% end

reply=zeros(1,256);  
while ~sum(reply([KEY.space KEY.esc])),
    WaitSecs(0.01);
    [keyIsDown, secs, reply] = KbCheck;
end
if reply(KEY.esc) 
    fprintf(fid,'...aborted\n');
    sca; 
    clear all;
    return; 
end 
%% Start examples 
[DATA] = sequence(DATA, D, KEY, fid, logfile, 0); 

Screen('DrawText',D.window,'Well done! Now the experiment starts.',D.hstart,D.vstart,D.textcolor);  

Screen('DrawText',D.window,'Press <space> to start and <esc> to abort.',D.hstart,D.vstart+2*D.vspace,D.textcolor);
Screen('Flip',D.window);
   
while KbCheck
end
 
reply=zeros(1,256);
while ~sum(reply([KEY.space KEY.esc])),
    WaitSecs(0.01);
    [keyIsDown, secs, reply] = KbCheck;
end
if reply(KEY.esc)
    fprintf(fid,'...aborted\n');
    sca;
    clear all;
    return;
end

%% Start bo trial loop

DATA.t_start_experiment = GetSecs;  % record the time of the beginning of the trial sequence

[DATA] = sequence(DATA, D, KEY, fid, logfile, 1);

%% Summary data

% end of block message to subject
Screen('DrawText',D.window,'Thank you',D.hstart,D.vstart,D.textcolor);
Screen('Flip',D.window);
WaitSecs(3);

%% RETURN TO MATLAB MODE
Priority(0);
sca;ShowCursor;
clear all;fclose all;
return  
        
      