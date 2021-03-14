function [DATA] = sequence(DATA, D, KEY, fid, logfile, experiment)

switch experiment
    case 0
        DATA.n_trials = length(DATA.examples);
    case 1
        DATA.n_trials = length(DATA.stimulus);
end

for trial = 1 : DATA.n_trials

    trial_start = GetSecs;  % record the current time for trial timing

        
    switch experiment
        case 0
            stimulus =  ['Stimuli/',DATA.examples{trial},'.jpg'];% image
            %name_separate = [DATA.firstname(trial),' ',DATA.middlename(trial),' ',DATA.lastname(trial)];
            %name = cell2mat(name_separate);
            name = cell2mat(DATA.examplesName);

        case 1
            which_row = DATA.random_order(trial);  % pick the next value from the randomized sequence defined previously
            stimulus = ['Stimuli/',DATA.stimulus{which_row},'.jpg'];  % get the stimulus1 for this trial. Note the use of braces {}
            DATA.exp_data(trial,4) = DATA.pair(which_row);
            name_separate = [DATA.firstname(which_row),' ',DATA.middlename(which_row),' ',DATA.lastname(which_row)];
  %          name_separate = [DATA.firstname(which_row),' ',DATA.lastname(which_row)];
            name = cell2mat(name_separate);
     
    end
        
    %get width and height of stimulus
    infoPic = imfinfo(stimulus);
    D.rs=infoPic.Height;
    D.cs=infoPic.Width;
    D.rect=[0 0 D.cs D.rs];%stimulus window
    D.rect_center = D.rect + [D.screensize_c/2-D.cs/2 D.screensize_r/2-D.rs/2 D.screensize_c/2-D.cs/2 D.screensize_r/2-D.rs/2];
    DATA.presentationPosition = 'center';
    
 for i=[1:1:7]
    % load pictures & make textures      
    image=imread(stimulus);
    stim =Screen('MakeTexture',D.window,image);
    
    msg=['Answer: '];
    
    if i==1,
        ques = ['What is your social distance with this person (1-very close, 7-very far, 0-dont know)?'];
    else
        if i ==2,
        ques = ['What is your temporal distance with this person (1-very close, 7-very far, 0-dont know)?'];
        else
            if i ==3,
        ques = ['What is your spatial distance with this person (1-very close, 7-very far, 0-dont know)?'];
            else
                if i ==4,
        ques = ['                                         What is this person known for?'];
                else
                    if i ==5,
        ques = ['                         What time period is most associated with this person?                    '];
                    else
                        if i ==6,
        ques = ['                            What place is this person mostly associated with?'];
                        else
                             ques = ['Do you have a personal memory associated with this famous person? If so, what is that memory?'];
 
                    end;
                end;
            end;
        end;
    end;
    end;
          
       
    
    response = '';
    output = [msg response];
    draw_Confidence(D, output, stim, ques, name);
    
    FlushEvents('keyDown');

    while strcmp(response,''); %need this loop to prevent blank responses
    draw_Confidence(D, output, stim, ques, name);
    Screen('Flip', D.window);

    [secs, reply, deltaSecs] = KbWait;

        if reply(KEY.esc)
            fprintf(fid,'...aborted\n');
            sca;
            clear all;
            return;  
        end

  
        while true; %this loops breaks only when ENTER is pressed
            char = GetChar;
  
            switch (abs(char))
                case {13, 3, 10} %this is the ENTER button
                    break;
                case 8 %this is for delete button
                    if ~isempty(response)
                        response = response(1:length(response)-1);
                    end
                    output = [msg response];
                    draw_Confidence(D, output, stim, ques, name);
                    Screen('Flip', D.window);
                otherwise
                    response = [response, char];
            end
            output = [msg response];
            draw_Confidence(D, output, stim, ques, name);
            Screen('Flip', D.window);
        end

    end
    if reply(KEY.esc)
        fprintf(fid,'...aborted\n');
        sca; ShowCursor;
        clear all;fclose all;
        return
    elseif experiment == 1
        %logging: columns 'condition','stimulus1','stimulus2,'time_present',
        %'time_keypress','key','RT','corr'
          
        DATA.exp_data(trial,1) = trial;   % which trial
        DATA.exp_data(trial,2) = which_row;   % which line of the input file
        DATA.exp_data(trial,3) = DATA.number(which_row);  % stimulus number
             
         fprintf(fid,'%d\t%s\t%d\t%s\n',...
             DATA.exp_data(trial,1), DATA.stimulus{which_row},DATA.exp_data(trial,3),response);

        
        %Save data in .mat; strrep replaces all occurrences of .log with .mat in
        %logfile
        save(strrep(logfile,'.log','.mat'), 'DATA');  % save the whole structure to the subject's data file
    else
    end
            
    end
end
        